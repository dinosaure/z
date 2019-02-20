open Ctypes
open Foreign

let w = Z.bigstring_create Z.Window.max

let inflate i i_len r r_len =
  let i = bigarray_of_ptr array1 i_len Bigarray.char i in
  let i = Bigarray.Array1.sub i 2 (i_len - 2) in
  let r = bigarray_of_ptr array1 r_len Bigarray.char r in
  let decoder = Z.M.decoder `Manual r w in
  let rec go () = match Z.M.decode decoder with
    | `Await -> assert false
    | `Flush -> go ()
    | `End -> r_len - Z.M.dst_rem decoder
    | `Malformed err -> invalid_arg err in
  Z.M.src decoder i 0 (i_len - 2) ; go ()

module Stubs (I : Cstubs_inverted.INTERNAL) = struct
  let () =
    I.internal "miniz_inflate"
      (ptr char @-> int @-> ptr char @-> int @-> returning int)
      inflate
end
