let io_buffer_size = 65536

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type window = Dd.window

let bigstring_create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l
let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_length x = Bigarray.Array1.dim x [@@inline]

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_set_uint32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external unsafe_set_uint16 : bigstring -> int -> int -> unit = "%caml_bigstring_set16"

external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "caml_int32_bswap"

external string_unsafe_get_uint32 : string -> int -> int32 = "%caml_string_get32"
external bytes_unsafe_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"
let string_unsafe_get_uint8 : string -> int -> int =
  fun buf off -> Char.code (String.get buf off)
let bytes_unsafe_get_uint8 : bytes -> int -> int =
  fun buf off -> Char.code (Bytes.get buf off)
external bytes_unsafe_set_uint32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32"
let bytes_unsafe_set_uint8 : bytes -> int -> int -> unit =
  fun buf off v -> Bytes.set buf off (Char.unsafe_chr (v land 0xff))

let input_bigstring ic buf off len =
  let tmp = Bytes.create len in
  let res = input ic tmp 0 len in

  let len0 = res land 3 in
  let len1 = res asr 2 in

  for i = 0 to len1 - 1
  do
    let i = i * 4 in
    let v = bytes_unsafe_get_uint32 tmp i in
    unsafe_set_uint32 buf (off + i) v
  done ;

  for i = 0 to len0 - 1
  do
    let i = len1 * 4 + i in
    let v = bytes_unsafe_get_uint8 tmp i in
    unsafe_set_uint8 buf (off + i) v
  done ; res

let bigstring_to_string v =
  let len = bigstring_length v in
  let res = Bytes.create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1
  do
    let i = i * 4 in
    let v = unsafe_get_uint32 v i in
    bytes_unsafe_set_uint32 res i v
  done ;

  for i = 0 to len0 - 1
  do
    let i = len1 * 4 + i in
    let v = unsafe_get_uint8 v i in
    bytes_unsafe_set_uint8 res i v
  done ;

  Bytes.unsafe_to_string res

let output_bigstring oc buf off len =
  (* XXX(dinosaure): stupidly slow! *)
  let v = Bigarray.Array1.sub buf off len in
  let v = bigstring_to_string v in
  output_string oc v

let bigstring_of_string v =
  let len = String.length v in
  let res = bigstring_create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1
  do
    let i = i * 4 in
    let v = string_unsafe_get_uint32 v i in
    unsafe_set_uint32 res i v
  done ;

  for i = 0 to len0 - 1
  do
    let i = len1 * 4 + i in
    let v = string_unsafe_get_uint8 v i in
    unsafe_set_uint8 res i v
  done ; res

let unsafe_get_uint32 =
  if Sys.big_endian
  then fun buf off -> unsafe_get_uint32 buf off
  else fun buf off -> swap32 (unsafe_get_uint32 buf off)

let unsafe_set_uint32 =
  if Sys.big_endian
  then fun buf off v -> unsafe_set_uint32 buf off v
  else fun buf off v -> unsafe_set_uint32 buf off (swap32 v)

let unsafe_set_uint16 =
  if Sys.big_endian
  then fun buf off v -> unsafe_set_uint16 buf off v
  else fun buf off v -> unsafe_set_uint16 buf off (swap16 v)

let invalid_bounds off len = Fmt.invalid_arg "Out of bounds (off: %d, len: %d)" off len

let _deflated = 8 (* Compression method *)

module M = struct
  type src = [ `Channel of in_channel | `String of string | `Manual ]

  (* XXX(dinosaure): immutable style. *)
  type decoder =
    { src : Dd.M.src
    ; i : bigstring
    ; i_pos : int
    ; i_len : int
    ; wr : int
    ; hd : int
    ; dd : dd
    ; fdict : bool
    ; flevel : int
    ; cinfo : int
    ; k : decoder -> decode }
  and dd =
    | Dd of { state : Dd.M.decoder
            ; window : Dd.window
            ; o : Dd.bigstring }
    | Hd of { allocate : int -> Dd.window
            ; o : Dd.bigstring }
  and decode =
    [ `Await of decoder
    | `Flush of decoder
    | `End of decoder
    | `Malformed of string ]

  let malformedf fmt = Fmt.kstrf (fun s -> `Malformed s) fmt

  let err_unexpected_end_of_input _ =
    malformedf "Unexpected end of input"

  let err_invalid_checksum has expect _ =
    malformedf "Invalid checksum (expect:%04lx, has:%04lx)" expect (Optint.to_int32 has)

  let err_invalid_header _ =
    malformedf "Invalid header"

  let i_rem d = d.i_len - d.i_pos + 1

  let eoi d =
    { d with i= bigstring_empty
           ; i_pos= 0
           ; i_len= min_int }

  let refill k d = match d.dd, d.src with
    | Dd { state; _ }, `String _ ->
      Dd.M.src state bigstring_empty 0 0 ;
      k (eoi d)
    | Dd { state; _ }, `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      Dd.M.src state d.i 0 res ; k d
    | (Dd _ | Hd _), `Manual ->
      `Await { d with k }
    | Hd _, `String _ ->
      k (eoi d)
    | Hd _, `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      if res == 0
      then k (eoi d)
      else k { d with i_pos= 0; i_len= res - 1 }

  let flush k d = `Flush { d with k }

  let rec checksum d =
    let k d = match d.dd with
      | Dd { state; _ } ->
        let a = Dd.M.checksum state in
        let b = unsafe_get_uint32 d.i d.i_pos in

        if Optint.to_int32 a = b (* FIXME: Optint.equal a (Optint.of_int32 b) bugs! *)
        then `End { d with i_pos= d.i_pos + 4 }
        else err_invalid_checksum a b d
      | Hd _ -> assert false in

    if i_rem d >= 4
    then k d
    else ( if i_rem d < 0 then err_unexpected_end_of_input d else refill checksum d )

  let rec header d =
    let k d = match d.dd with
      | Hd { allocate; o; } ->
        let cmf = unsafe_get_uint16 d.i d.i_pos in
        let cm = cmf land 0b1111 in
        let cinfo = (cmf lsr 4) land 0b1111 in
        let flg = cmf lsr 8 in
        let fdict = (flg lsr 5) land 0b1 in
        let flevel = (flg lsr 6) land 0b11 in
        let window = allocate (cinfo + 8) in
        let state = Dd.M.decoder `Manual ~o ~w:window in
        let dd = Dd { state; window; o; } in
        if ((cmf land 0xff) lsl 8 + (cmf lsr 8)) mod 31 != 0
           || cm != _deflated
        then err_invalid_header d
        else
          ( Dd.M.src state d.i (d.i_pos + 2) (i_rem { d with i_pos= d.i_pos + 2 })
          ; decode { d with hd= unsafe_get_uint16 d.i d.i_pos
                          ; dd
                          ; fdict= fdict == 1; flevel; cinfo
                          ; i_pos= d.i_pos + 2 } )
      | Dd _ ->
        assert false (* XXX(dinosaure): should never occur! *) in
    if i_rem d >= 2
    then k d else ( if i_rem d < 0 then err_unexpected_end_of_input d else refill header d )

  and decode d = match d.dd with
    | Hd _ -> header d
    | Dd { state; o; _ } ->
      match Dd.M.decode state with
      | `Flush ->
        let len = bigstring_length o - Dd.M.dst_rem state in
        (* XXX(dinosaure): protect counter to a recall? TODO *)
        `Flush { d with wr= d.wr + len }
      | `Await -> refill decode d
      | `End ->
        let len = bigstring_length o - Dd.M.dst_rem state in
        if len > 0
        then flush checksum { d with wr= d.wr + len }
        else checksum { d with i_pos= d.i_pos + (i_rem d - Dd.M.src_rem state) }
      | `Malformed err -> `Malformed err

  let src d s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    let d =
      if (l == 0)
      then eoi d
      else
        { d with i= s
               ; i_pos= j
               ; i_len= j + l - 1 } in
    match d.dd with
    | Dd { state; _ } ->
      Dd.M.src state s j l ; d
    | Hd _ -> d

  let flush d = match d.dd with
    | Hd _ -> Fmt.invalid_arg "Invalid state to flush"
    | Dd { state; _ } ->
      Dd.M.flush state ; d

  let dst_rem d = match d.dd with
    | Hd _ -> Fmt.invalid_arg "Invalid state to know bytes remaining"
    | Dd { state; _ } -> Dd.M.dst_rem state

  let write { wr; _ } = wr

  let decoder src ~o ~allocate =
    let i, i_pos, i_len = match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    { i; i_pos; i_len
    ; src
    ; wr= 0
    ; hd= 0
    ; dd= Hd { allocate; o; }
    ; fdict= false
    ; flevel= 2
    ; cinfo= 8
    ; k= decode }
end

module N = struct
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type encoder =
    { src : src
    ; dst : dst
    ; level : int
    ; i : bigstring
    ; i_pos : int
    ; i_len : int
    ; o : bigstring
    ; o_pos : int
    ; o_len : int
    ; q : Dd.B.t
    ; s : Dd.L.state
    ; e : Dd.N.encoder
    ; w : Dd.window
    ; state : state
    ; k : encoder -> [ `Await of encoder | `Flush of encoder | `End of encoder ] }
  and state = Hd | Dd

  type ret = [ `Await of encoder | `End of encoder | `Flush of encoder ]

  let o_rem e = e.o_len - e.o_pos + 1
  let i_rem s = s.i_len - s.i_pos + 1

  let eoi e =
    { e with i= bigstring_empty
           ; i_pos= 0
           ; i_len= min_int }

  let src e s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    Dd.L.src e.s s j l ;
    if (l == 0) then eoi e
    else { e with i= s; i_pos= j; i_len= j + l - 1 }

  let dst e s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    ( ( match e.state with
          | Hd -> ()
          | Dd -> Dd.N.dst e.e s j l )
    ; { e with o= s; o_pos= j; o_len= j + l - 1 } )

  let refill k e = match e.src with
    | `String _ -> k (eoi e)
    | `Channel ic ->
      let res = input_bigstring ic e.i 0 (bigstring_length e.i) in
      k (src e e.i 0 res)
    | `Manual -> `Await { e with k }

  let flush k e = match e.dst with
    | `Buffer b ->
      for i = 0 to bigstring_length e.o - Dd.N.dst_rem e.e
      do Buffer.add_char b e.o.{i} done ;
      k (dst e e.o 0 (bigstring_length e.o))
    | `Channel oc ->
      output_bigstring oc e.o 0 (bigstring_length e.o - Dd.N.dst_rem e.e) ;
      k (dst e e.o 0 (bigstring_length e.o))
    | `Manual -> `Flush { e with k }

  let identity e = `End e

  let rec checksum e =
    let k e =
      let checksum = Optint.to_int32 (Dd.L.checksum e.s) in
      unsafe_set_uint32 e.o e.o_pos checksum ;
      `End { e with k= identity; o_pos= e.o_pos + 4 } in
    if o_rem e >= 4 then k e else refill checksum e

  let make_block ?(last= false) e =
    if last = false
       then
         let literals = Dd.L.literals e.s in
         let distances = Dd.L.distances e.s in
         let dynamic = Dd.N.dynamic_of_frequencies ~literals ~distances in
         { Dd.N.kind= Dd.N.Dynamic dynamic; last; }
       else { Dd.N.kind= Dd.N.Fixed; last; }

  let rec encode e = match e.state with
    | Hd ->
      let k e =
        let header = (_deflated + ((Dd.window_bits e.w - 8) lsl 4)) lsl 8 in
        let header = header lor (e.level lsl 6) in
        let header = header + (31 - (header mod 31)) in
        unsafe_set_uint16 e.o e.o_pos header ;
        if i_rem e > 0 then Dd.L.src e.s e.i e.i_pos (i_rem e) ;
        (* XXX(dinosaure): we need to protect [e.s] against EOI signal. *)
        Dd.N.dst e.e e.o (e.o_pos + 2) (o_rem e - 2) ;
        encode { e with state= Dd; o_pos= e.o_pos + 2 } in
      if o_rem e >= 2 then k e else flush encode e
    | Dd ->
      let rec partial k e =
        k e (Dd.N.encode e.e `Await)
      and compress e =
        match Dd.L.compress e.s with
        | `Await ->
          refill compress { e with i_pos= e.i_pos + (i_rem e - Dd.L.src_rem e.s) }
        | `Flush ->
          encode_deflate e (Dd.N.encode e.e `Flush)
        | `End ->
          Dd.B.push_exn e.q Dd.B.eob ;
          let block = make_block ~last:true e in
          trailing e (Dd.N.encode e.e (`Block block))
      and encode_deflate e = function
        | `Partial ->
          let len = o_rem e - Dd.N.dst_rem e.e in
          flush (partial encode_deflate) { e with o_pos= e.o_pos + len }
        | `Ok ->
          compress e
        | `Block ->
          let block = make_block e in
          encode_deflate e (Dd.N.encode e.e (`Block block))
      and trailing e = function
        | `Partial ->
          let len = o_rem e - Dd.N.dst_rem e.e in
          flush (partial trailing) { e with o_pos= e.o_pos + len }
        | `Ok ->
          let len = o_rem e - Dd.N.dst_rem e.e in
          checksum { e with o_pos= e.o_pos + len }
        | `Block -> assert false (* XXX(dinosaure): should never occur! *) in

      compress e

  let src_rem = i_rem
  let dst_rem = o_rem

  let encoder src dst ~q ~w ~level =
    let i, i_pos, i_len = match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    let o, o_pos, o_len = match dst with
      | `Manual -> bigstring_empty, 1, 0
      | `Buffer _
      | `Channel _ -> bigstring_create io_buffer_size, 0, io_buffer_size - 1 in
    { src
    ; dst
    ; i; i_pos; i_len
    ; o; o_pos; o_len
    ; level
    ; e= Dd.N.encoder `Manual ~q
    ; s= Dd.L.state `Manual ~q ~w
    ; q
    ; w
    ; state= Hd
    ; k= encode }

  let encode e = e.k e
end
