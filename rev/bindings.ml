open Ctypes
open Foreign

let o_len = Z.io_buffer_size
let o = Bytes.create o_len

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external unsafe_set : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_get : bytes -> int -> int = "%bytes_unsafe_get"

let blit src src_off dst dst_off len =
  for i = 0 to len - 1
  do unsafe_set dst (dst_off + i) (unsafe_get src (src_off + i)) done

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp_string = pp_scalar ~get:String.get ~length:String.length

let inflate i i_len r r_len =
  let i = string_from_ptr i ~length:i_len in (* XXX(dinosaure): allocation here! *)
  let i = String.sub i 2 (i_len - 2) in
  let r = bigarray_of_ptr array1 r_len Bigarray.char r in
  let r_pos = ref 0 in
  let decoder = Z.M.decoder `Manual o in
  let rec go () = match Z.M.decode decoder with
    | `Await -> !r_pos
    | `Flush ->
      let len = o_len - Z.M.dst_rem decoder in
      blit o 0 r !r_pos len ; r_pos := !r_pos + len ; Z.M.flush decoder ; go ()
    | `End -> assert (Z.M.dst_rem decoder = o_len) ; !r_pos
    | `Malformed err -> invalid_arg err in
  Z.M.src decoder (Bytes.unsafe_of_string i) 0 (i_len - 2) ; go ()

module Stubs (I : Cstubs_inverted.INTERNAL) = struct
  let () =
    I.internal "miniz_inflate"
      (ptr char @-> int @-> ptr char @-> int @-> returning int)
      inflate
end
