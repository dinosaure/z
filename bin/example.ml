let literals = Z.N.make_literals ()
let () = Z.N.succ_literal literals '\000'
let () = Z.N.succ_literal literals '\000'
let () = Z.N.succ_length literals 258
let () = Z.N.succ_length literals 256
let distances = Z.N.make_distances ()
let () = Z.N.succ_distance distances 1
let () = Z.N.succ_distance distances 1

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

let encoder = Z.N.encoder (`Buffer res) (Z.N.Dynamic huffman)
let () = Fmt.epr "# dynamic huffman: done.\n%!"
let[@warning "-8"] `Ok : res = Z.N.encode encoder (`Literal '\000')
let () = Fmt.epr "# literal 00\n%!"
let[@warning "-8"] `Ok : res = Z.N.encode encoder (`Literal '\000')
let () = Fmt.epr "# literal 00\n%!"
let[@warning "-8"] `Ok : res = Z.N.encode encoder (`Copy (1, 258))
let () = Fmt.epr "#\n%!"
let[@warning "-8"] `Ok : res = Z.N.encode encoder (`Copy (1, 256))
let () = Fmt.epr "#\n%!"
let[@warning "-8"] `Ok : res = Z.N.encode encoder `End
let () = Fmt.epr "#\n%!"
let () = Fmt.pr "deflated: @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.O.default) (Buffer.contents res)
let () = Fmt.pr "deflated: %S\n%!" (Buffer.contents res)

(* We should have: "\xed\xc0\x01\x01\x00\x00\x00\x40\x20\xff\x57\x1b\x42\x2c\x4f" *)

let decoder = Z.M.decoder (`String (Buffer.contents res)) ~o ~w
let res = Z.M.decode decoder
let () = Fmt.pr "> %a.\n%!" pp_decode res
let () =
  if res = `End
  then let len = (Z.io_buffer_size - Z.M.dst_rem decoder) in
    let res = Bigstringaf.substring o ~off:0 ~len in
    Fmt.pr "inflated (len:%d): %S.\n%!" len res
