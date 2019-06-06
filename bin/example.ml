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
let q = Z.B.create 4096
let huffman = Z.N.dynamic_of_frequencies ~literals ~distances

let res = Buffer.create 16
type res = [ `Ok | `Partial | `End of Z.N.code ]

let pp_decode ppf = function
  | `Await -> Fmt.string ppf "`Await"
  | `End -> Fmt.string ppf "`End"
  | `Flush -> Fmt.string ppf "`Flush"
  | `Malformed err -> Fmt.pf ppf "(`Malformed %S)" err

let encoder = Z.N.encoder (`Buffer res) { kind= Z.N.Dynamic huffman; last= true; } ~q
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

let pp_decode ppf = function
  | `Await -> Fmt.string ppf "`Await"
  | `End -> Fmt.string ppf "`End"
  | `Flush -> Fmt.string ppf "`Flush"

let pp_chr = Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_code ppf = function
  | `Literal chr -> Fmt.pf ppf "(`Literal %02x:%a)" (Char.code chr) pp_chr chr
  | `Copy (off, len) -> Fmt.pf ppf "(`Copy off:%d len:%d)" off len

let () = Z.B.reset q
let () = Fmt.pr "> @[<hov>%a@].\n%!" Fmt.(Dump.list pp_code) (Z.B.to_list q)

let source = "salut les copains, je compresse quelque chose!"
let state = Z.L.state (`String source) ~q ~w
let ret : Z.L.decode = Z.L.compress state

let () = Fmt.pr "Got %a.\n%!" pp_decode ret
let () = Fmt.pr "> @[<hov>%a@].\n%!" Fmt.(Dump.list pp_code) (Z.B.to_list q)

let reconstruct lst =
  let len = List.fold_left (fun a -> function `Literal _ -> 1 + a | `Copy (_, len) -> len + a) 0 lst in
  let res = Bytes.create len in
  let pos = ref 0 in
  List.iter (function
      | `Literal chr -> Bytes.set res !pos chr ; incr pos
      | `Copy (off, len) ->
        for _ = 0 to len - 1
        do Bytes.set res !pos (Bytes.get res (!pos - off)) ; incr pos done)
    lst ;
  Bytes.unsafe_to_string res

let () = Fmt.epr "source == reconstruct? %b.\n%!" (String.equal source (reconstruct (Z.B.to_list q)))
