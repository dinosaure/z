let literals = Z.N.make_literals ()
let () = literals.(Char.code '\000') <- 1
let distances = Z.N.make_distances ()

let w = Z.bigstring_create Z.Window.max
let o = Z.bigstring_create Z.io_buffer_size
let huffman = Z.N.dynamic_of_frequencies ~literals ~distances

let res = Buffer.create 16
type res = [ `Ok | `Partial ]

let pp_decode ppf = function
  | `Await -> Fmt.string ppf "`Await"
  | `End -> Fmt.string ppf "`End"
  | `Flush -> Fmt.string ppf "`Flush"
  | `Malformed err -> Fmt.pf ppf "(`Malformed %S)" err

let encoder = Z.N.encoder (`Buffer res) (Z.N.Dynamic huffman) ~w
let[@warning "-8"] `Ok : res = Z.N.encode encoder (`Literal 0)
let[@warning "-8"] `Ok : res = Z.N.encode encoder (`Literal 0)
let[@warning "-8"] `Ok : res = Z.N.encode encoder (`Literal 0)
let[@warning "-8"] `Ok : res = Z.N.encode encoder `End
let () = Fmt.pr "deflated: %S\n%!" (Buffer.contents res)
let decoder = Z.M.decoder (`String (Buffer.contents res)) ~o ~w
let res = Z.M.decode decoder
let () = Fmt.pr "> %a.\n%!" pp_decode res
let () =
  if res = `End
  then let len = (Z.io_buffer_size - Z.M.dst_rem decoder) in
    let res = Bigstringaf.substring o ~off:0 ~len in
    Fmt.pr "inflated (len:%d): %S.\n%!" len res
