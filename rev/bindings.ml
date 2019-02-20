open Ctypes
open Foreign

let o_len = 0x800
let o = Bytes.create o_len

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external unsafe_set : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_get : bytes -> int -> int = "%bytes_unsafe_get"

let blit src src_off dst dst_off len =
  for i = 0 to len - 1
  do unsafe_set dst (dst_off + i) (unsafe_get src (src_off + i)) done

let inflate i i_len r r_len =
  let i = string_from_ptr i ~length:i_len in (* XXX(dinosaure): allocation here! *)
  let r = bigarray_of_ptr array1 r_len Bigarray.char r in
  let r_pos = ref 0 in
  let decoder = Z.M.decoder `Manual o in
  let rec go () = match Z.M.decode decoder with
    | `Await -> assert false
    | `Flush ->
      let len = o_len - Z.M.dst_rem decoder in
      blit o 0 r !r_pos len ; r_pos := !r_pos + len ; Z.M.flush decoder ; go ()
    | `End -> !r_pos
    | `Malformed err -> invalid_arg err in
  Z.M.src decoder (Bytes.unsafe_of_string i) 0 i_len ; go ()

module Stubs (I : Cstubs_inverted.INTERNAL) = struct
  let () =
    I.internal "miniz_inflate"
      (ptr char @-> int @-> ptr char @-> int @-> returning int)
      inflate
end
