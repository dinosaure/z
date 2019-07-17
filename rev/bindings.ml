open Ctypes
open Foreign
open Zz

let w = Dd.make_window ~bits:15
let o = Dd.bigstring_create Dd.io_buffer_size
let i = Dd.bigstring_create Dd.io_buffer_size
let q = Dd.B.create 8192

let inflate i i_len r r_len =
  let i = bigarray_of_ptr array1 i_len Bigarray.char i in
  let r = bigarray_of_ptr array1 r_len Bigarray.char r in
  let decoder = M.decoder `Manual ~o:r ~allocate:(fun _ -> w) in

  let rec decode decoder = match M.decode decoder with
    | `Await decoder -> M.src decoder Dd.bigstring_empty 0 0 |> pending
    | `Flush decoder -> M.flush decoder |> pending
    | `End decoder -> M.write decoder
    | `Malformed err -> invalid_arg err
  and pending decoder = match M.decode decoder with
    | `Await _ | `Flush _ -> failwith "Unexpected `Await or `Flush operation on an atomic inflate computation"
    | `End decoder -> M.write decoder
    | `Malformed err -> invalid_arg err in
  M.src decoder i 0 i_len |> decode

let deflate i i_len r r_len =
  let i = bigarray_of_ptr array1 i_len Bigarray.char i in
  let r = bigarray_of_ptr array1 r_len Bigarray.char r in
  let encoder = N.encoder `Manual `Manual ~q ~w ~level:0 in
  let r_pos = ref 0 in

  let rec encode encoder = match N.encode encoder with
    | `Await encoder -> N.src encoder Dd.bigstring_empty 0 0 |> pending
    | `Flush encoder ->
      let len = (r_len - !r_pos) - N.dst_rem encoder in
      r_pos := !r_pos + len ;
      N.dst encoder r !r_pos (r_len - !r_pos) |> encode
    | `End encoder ->
      let len = (r_len - !r_pos) - N.dst_rem encoder in
      !r_pos + len
  and pending encoder = match N.encode encoder with
    | `Await encoder -> failwith "Unexpected `Await operation on an atomic deflate computation"
    | `Flush encoder ->
      let len = (r_len - !r_pos) - N.dst_rem encoder in
      r_pos := !r_pos + len ;
      N.dst encoder r !r_pos (r_len - !r_pos) |> pending
    | `End encoder ->
      let len = (r_len - !r_pos) - N.dst_rem encoder in
      !r_pos + len in
  N.src encoder i 0 i_len |> fun encoder -> N.dst encoder r 0 r_len |> encode

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
