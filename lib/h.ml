[@@@warning "-32-34"]

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_length x = Bigarray.Array1.dim x [@@inline]
let bigstring_sub x off len = Bigarray.Array1.sub x off len [@@inline]
let bigstring_blit a b = Bigarray.Array1.blit a b [@@inline]
let bigstring_create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_char : bigstring -> int -> char = "%caml_ba_ref_1"
external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_set_char : bigstring -> int -> char -> unit = "%caml_ba_set_1"

external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_set_uint32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"

let unsafe_blit src src_off dst dst_off len =
  let a = bigstring_sub src src_off len in
  let b = bigstring_sub dst dst_off len in
  bigstring_blit a b

let unsafe_blit_from_string src src_off dst dst_off len =
  for i = 0 to len - 1
  do unsafe_set_char dst (dst_off + i) (String.get src (src_off + i)) done

let invalid_bounds off len = Fmt.invalid_arg "Out of bounds (off: %d, len: %d)" off len
let invalid_encode () = Fmt.invalid_arg "expected `Await encode"

let output_bigstring _ _ _ _ = ()

let slow_blit src src_off dst dst_off len =
  for i = 0 to len - 1
  do
    let v = unsafe_get_uint8 src (src_off + i) in
    unsafe_set_uint8 dst (dst_off + i) v ;
  done

let blit src src_off dst dst_off len =
  if dst_off - src_off < 4
  then slow_blit src src_off dst dst_off len
  else
    let len0 = len land 3 in
    let len1 = len asr 2 in

    for i = 0 to len1 - 1
    do
      let i = i * 4 in
      let v = unsafe_get_uint32 src (src_off + i) in
      unsafe_set_uint32 dst (dst_off + i) v ;
    done ;

    for i = 0 to len0 - 1
    do
      let i = len1 * 4 + i in
      let v = unsafe_get_uint8 src (src_off + i) in
      unsafe_set_uint8 dst (dst_off + i) v ;
    done

let io_buffer_size = 65536

external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let ( > ) (x : int) y = x > y [@@inline]
let ( < ) (x : int) y = x < y [@@inline]
let ( <= ) (x : int) y = x <= y [@@inline]

module M = struct
  type decode = [ `Await | `Destination of int | `End | `Malformed of string ]

  type decoder =
    { src : bigstring
    ; mutable dst : bigstring
    ; mutable i : bigstring
    ; mutable i_pos : int
    ; mutable i_len : int
    ; mutable o_pos : int
    ; mutable src_len : int
    ; mutable dst_len : int
    ; mutable s : state }
  and ret = Await | Stop | End | Malformed of string
  and state = Header | Dst | Cmd | Cp of int | It of int

  let variable_length buf off top =
    let p = ref off in
    let i = ref 0 in
    let len = ref 0 in

    while ( let cmd = unsafe_get_uint8 buf !p in
            incr p
          ; len := !len lor ((cmd land 0x7f) lsl !i)
          ; i := !i + 7
          ; cmd land 0x80 = 0 && !p <= top )
    do () done ; (!p - off, !len)
  [@@inline]

  let eoi d =
    d.i <- bigstring_empty ;
    d.i_pos <- 0 ;
    d.i_len <- min_int

  let i_rem d = d.i_len - d.i_pos + 1
  [@@inline]

  let src_rem = i_rem
  let dst_rem d = d.dst_len - d.o_pos

  let malformedf fmt = Fmt.kstrf (fun s -> Malformed s) fmt

  let src d s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    if (l == 0) then eoi d
    else
      ( d.i <- s
      ; d.i_pos <- j
      ; d.i_len <- j + l - 1 )

  let dst d s j l = match d.s with
    | Dst ->
      if (j < 0 || l < 0 || j + l > bigstring_length s)
      then invalid_bounds j l
      else if l <> d.dst_len
      then Fmt.invalid_arg "Invalid destination"
      else ( d.dst <- s
           ; d.o_pos <- j
           ; d.s <- Cmd )
    | _ -> Fmt.invalid_arg "Invalid call of dst"

  let required =
    let a = [| 0; 1; 1; 2; 1; 2; 2; 3; 1; 2; 2; 3; 2; 3; 3; 4 |] in
    fun x -> a.(x land 0xf) + a.(x lsr 4) - 1 (* XXX(dinosaure): uncount 0x80 *)

  let enough d = match d.s with
    | Cp cmd -> i_rem d >= required cmd
    | It len -> i_rem d >= len
    | _ -> assert false (* XXX(dinosaure): [enough] is called only after a [d.s <- (It _ | Cp _)]. *)

  (* XXX(dinosaure): [flambda] is able to optimize [let rec a .. and b .. and c ..]
     instead [match .. with A -> .. | B -> .. | C -> ..]. *)

  let rec cp d =
    let[@warning "-8"] Cp command = d.s in
    let p = ref d.i_pos in
    let cp_off = ref 0 in
    let cp_len = ref 0 in

    if command land 0x01 != 0
    then ( let v = unsafe_get_uint8 d.i !p in
           cp_off := v
         ; incr p ) ;
    if command land 0x02 != 0
    then ( let v = unsafe_get_uint8 d.i !p in
           cp_off := !cp_off lor (v lsl 8)
         ; incr p ) ;
    if command land 0x04 != 0
    then ( let v = unsafe_get_uint8 d.i !p in
           cp_off := !cp_off lor (v lsl 16)
         ; incr p ) ;
    if command land 0x08 != 0
    then ( let v = unsafe_get_uint8 d.i !p in
           cp_off := !cp_off lor (v lsl 24)
         ; incr p ) ;
    if command land 0x10 != 0
    then ( let v = unsafe_get_uint8 d.i !p in
           cp_len := v
         ; incr p ) ;
    if command land 0x20 != 0
    then ( let v = unsafe_get_uint8 d.i !p in
           cp_len := !cp_len lor (v lsl 8)
         ; incr p ) ;
    if command land 0x40 != 0
    then ( let v = unsafe_get_uint8 d.i !p in
           cp_len := !cp_len lor (v lsl 16)
         ; incr p ) ;

    blit d.src !cp_off d.dst d.o_pos !cp_len ;
    d.i_pos <- !p ;
    d.o_pos <- d.o_pos + !cp_len ;
    d.s <- Cmd ;
    cmd d

  and it d =
    let[@warning "-8"] It len = d.s in
    blit d.i d.i_pos d.dst d.o_pos len ;
    d.i_pos <- d.i_len + len ;
    d.o_pos <- d.o_pos + len ;
    d.s <- Cmd ;
    cmd d

  and cmd d =
    let cmd = unsafe_get_uint8 d.i d.i_pos in

    if cmd == 0 then malformedf "Invalid delta code (%02x)" cmd
    else
      ( d.s <- if cmd land 0x80 != 0 then Cp cmd else It cmd
      ; d.i_pos <- d.i_pos + 1

      ; if enough d
        then ( if cmd land 0x80 != 0 then cp d else it d )
        else Await )

  let decode_k d =
    let rem = i_rem d in

    if rem <= 0
    then ( if rem < 0 then End else Await )
    else match d.s with
    | Header ->
      if rem < 4 then Fmt.invalid_arg "Not enough space" ; (* TODO: [malformedf]? *)
      let x, src_len = variable_length d.i d.i_pos d.i_len in
      let y, dst_len = variable_length d.i (d.i_pos + x) d.i_len in

      d.i_pos <- d.i_pos + x + y ;
      d.src_len <- src_len ;
      d.dst_len <- dst_len ;

      if d.src_len != bigstring_length d.src then malformedf "Invalid source"
      else ( d.s <- Dst ; Stop )
    | Dst -> Stop
    | Cmd -> cmd d
    | Cp cmd -> if required cmd <= rem then cp d else Await
    | It len -> if len <= rem then cp d else Await

  let decode d = match decode_k d with
    | Await -> `Await
    | Stop -> `Destination d.dst_len
    | End -> `End
    | Malformed err -> `Malformed err

  let decoder src =
    let i, i_pos, i_len = bigstring_empty, 1, 0 in
    { src
    ; dst= bigstring_empty
    ; i
    ; i_pos
    ; i_len
    ; o_pos= 0
    ; src_len= 0
    ; dst_len= 0
    ; s= Header }
end

module N = struct
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  type encode = [ `Await | `Copy of int * int | `Insert of string | `End ]

  type encoder =
    { dst : dst
    ; mutable o : bigstring
    ; mutable o_pos : int
    ; mutable o_max : int
    ; t : bigstring (* XXX(dinosaure): normalize to [bytes]? *)
    ; mutable t_pos : int
    ; mutable t_max : int
    ; mutable k : encoder -> encode -> [ `Ok | `Partial ] }

  let o_rem e = e.o_max - e.o_pos + 1 [@@inline]

  let dst e s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    e.o <- s ;
    e.o_pos <- j ;
    e.o_max <- j + l - 1

  let partial k e = function
    | `Await -> k e
    | `Copy _ | `Insert _ | `End -> invalid_encode ()

  let flush k e = match e.dst with
    | `Manual -> e.k <- partial k ; `Partial
    | `Channel oc -> output_bigstring oc e.o 0 e.o_pos ; e.o_pos <- 0 ; k e
    | `Buffer b ->
      (* XXX(dinosaure): optimize it! *)
      for i = 0 to e.o_pos - 1 do Buffer.add_char b (unsafe_get_char e.o i) done ;
      e.o_pos <- 0 ; k e

  let cmd off len =
    let cmd = ref 0 in

    if off land 0x000000ff <> 0 then cmd := !cmd lor 0x01 ;
    if off land 0x0000ff00 <> 0 then cmd := !cmd lor 0x02 ;
    if off land 0x00ff0000 <> 0 then cmd := !cmd lor 0x04 ;
    if off land 0xff000000 <> 0 then cmd := !cmd lor 0x08 ;

    if len land 0x0000ff <> 0 then cmd := !cmd lor 0x10 ;
    if len land 0x00ff00 <> 0 then cmd := !cmd lor 0x20 ;
    if len land 0xff0000 <> 0 then cmd := !cmd lor 0x40 ;

    !cmd
  [@@inline]

  let t_range e max =
    e.t_pos <- 0 ;
    e.t_max <- max

  let rec t_flush k e =
    let blit e l =
      unsafe_blit e.t e.t_pos e.o e.o_pos l ;
      e.o_pos <- e.o_pos + l ; e.t_pos <- e.t_pos + l in
    let rem = o_rem e in
    let len = e.t_max - e.t_pos + 1 in

    if rem < len
    then ( blit e rem ; flush (t_flush k) e )
    else ( blit e len ; k e )

  let rec encode e v =
    let k e = e.k <- encode ; `Ok in
    match v with
    | `Await -> k e
    | `Copy (off, len) ->
      let rem = o_rem e in
      let cmd = cmd off len in
      let required = M.required cmd in

      let s, j, k =
        if rem < required
        then ( t_range e (required - 1) ; e.t, 0, t_flush k )
        else let j = e.o_pos in ( e.o_pos <- e.o_pos + required - 1 ; e.o, j, k ) in

      unsafe_set_uint8 s j (cmd lor 0x80) ;
      let pos = ref (j + 1) in
      let off = ref off in while !off <> 0 do unsafe_set_uint8 s !pos !off ; incr pos ; off := !off asr 8 done ;
      let len = ref len in while !len <> 0 do unsafe_set_uint8 s !pos !len ; incr pos ; len := !len asr 8 done ;
      k e
    | `Insert p ->
      let rem = o_rem e in
      let len = String.length p in
      let s, j, k =
        if rem < len + 1
        then ( t_range e len ; e.t, 0, t_flush k )
        else let j = e.o_pos in ( e.o_pos <- e.o_pos + len ; e.o, j, k ) in

      unsafe_set_uint8 s j len ;
      unsafe_blit_from_string p 0 s (j + 1) len ;
      k e
    | `End -> flush k e

  let encoder dst =
    let o, o_pos, o_max = match dst with
      | `Manual -> bigstring_empty, 1, 0
      | `Buffer _
      | `Channel _ -> bigstring_create io_buffer_size, 0, io_buffer_size - 1 in
    { dst
    ; o; o_pos; o_max
    ; t= bigstring_create 128
    ; t_pos= 1; t_max= 0
    ; k= encode }
end
