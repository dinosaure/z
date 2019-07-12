let io_buffer_size = 65536

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type window = Dd.window

let bigstring_create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l
let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_length x = Bigarray.Array1.dim x [@@inline]

external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_set_uint32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"

external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external swap32 : int32 -> int32 = "caml_int32_bswap"

let unsafe_get_uint32 =
  if Sys.big_endian
  then fun buf off -> unsafe_get_uint32 buf off
  else fun buf off -> swap32 (unsafe_get_uint32 buf off)

external string_unsafe_get_uint32 : string -> int -> int32 = "%caml_string_get32"
external bytes_unsafe_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"
let string_unsafe_get_uint8 : string -> int -> int =
  fun buf off -> Char.code (String.get buf off)
let bytes_unsafe_get_uint8 : bytes -> int -> int =
  fun buf off -> Char.code (Bytes.get buf off)

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

  let refill k d = match d.dd, d.src with
    | Dd { state; _ }, `String _ ->
      Dd.M.src state bigstring_empty 0 0 ; k d
    | Dd { state; _ }, `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      Dd.M.src state d.i 0 res ; k d
    | (Dd _ | Hd _), `Manual ->
      `Await { d with k }
    | Hd _, `String _ ->
      k { d with i= bigstring_empty
               ; i_pos= 0
               ; i_len= min_int }
    | Hd _, `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      if res == 0
      then k { d with i= bigstring_empty
                    ; i_pos= 0
                    ; i_len= min_int }
      else k { d with i_pos= 0; i_len= res - 1 }

  let flush k d = `Flush { d with k }

  let rec checksum d =
    let k d = match d.dd with
      | Dd { state; _ } ->
        let a = Dd.M.checksum state in
        let b = unsafe_get_uint32 d.i d.i_pos in
        if Optint.equal a (Optint.of_int32 b)
        then `End { d with i_pos= d.i_pos + 4 }
        else err_invalid_checksum a b d
      | Hd _ -> assert false in
    if i_rem d >= 4
    then k d else ( if i_rem d < 0 then err_unexpected_end_of_input d else refill checksum d )

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
      | `Flush -> `Flush d
      | `Await -> refill decode d
      | `End ->
         if bigstring_length o - Dd.M.dst_rem state > 0
         then flush checksum d
         else checksum { d with i_pos= d.i_pos + (i_rem d - Dd.M.src_rem state) }
      | `Malformed err -> `Malformed err

  let src d s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    let d =
      if (l == 0)
      then { d with i= bigstring_empty
                  ; i_pos= 0
                  ; i_len= min_int }
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

  let decoder src ~o ~allocate =
    let i, i_pos, i_len = match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    { i; i_pos; i_len
    ; src
    ; hd= 0
    ; dd= Hd { allocate; o; }
    ; fdict= false
    ; flevel= 2
    ; cinfo= 8
    ; k= decode }
end
