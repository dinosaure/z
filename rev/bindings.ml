open Ctypes
open Foreign

let w = Z.bigstring_create Z.Window.max
let q = Z.B.create 4096

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

let deflate i i_len r r_len =
  let i = bigarray_of_ptr array1 i_len Bigarray.char i in
  let r = bigarray_of_ptr array1 r_len Bigarray.char r in
  let state = Z.L.state `Manual ~w ~q in
  let encoder = Z.N.encoder `Manual ~q in
  let kind = ref Z.N.Fixed in

  Z.L.src state i 0 i_len ;
  Z.N.dst encoder r 0 r_len ;

  let rec compress () = match Z.L.compress state with
    | `Await -> Z.L.src state i 0 0 ; compress ()
    | `End -> Z.B.push_exn q Z.B.eob ; finish (Z.N.encode encoder (`Block { Z.N.kind= Z.N.Fixed; last= true }))
    | `Flush ->
      let literals = Z.L.literals state in
      let distances = Z.L.distances state in
      kind := Z.N.Dynamic (Z.N.dynamic_of_frequencies ~literals ~distances) ;
      encode (Z.N.encode encoder (`Block { Z.N.kind= !kind; last= false }))
  and encode = function
    | `Partial -> assert false
    | `Ok -> compress ()
    | `Block ->
      let literals = Z.L.literals state in
      let distances = Z.L.distances state in
      kind := Z.N.Dynamic (Z.N.dynamic_of_frequencies ~literals ~distances) ;
      encode (Z.N.encode encoder (`Block { Z.N.kind= !kind; last= false }))
  and finish = function
    | `Partial | `Block -> assert false
    | `Ok -> r_len - Z.N.dst_rem encoder in
  compress ()

module Stubs (I : Cstubs_inverted.INTERNAL) = struct
  let () =
    I.internal "miniz_inflate"
      (ptr char @-> int @-> ptr char @-> int @-> returning int)
      inflate
  
  let () =
    I.internal "miniz_deflate"
      (ptr char @-> int @-> ptr char @-> int @-> returning int)
      deflate
end
