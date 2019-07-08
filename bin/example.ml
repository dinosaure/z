let literals = Z.make_literals ()
let () = Z.succ_literal literals '\000'
let () = Z.succ_literal literals '\000'
let () = Z.succ_length literals 258
let () = Z.succ_length literals 256
let distances = Z.make_distances ()
let () = Z.succ_distance distances 1
let () = Z.succ_distance distances 1

let w = Z.make_window ~bits:15
let o = Z.bigstring_create Z.io_buffer_size
let q = Z.B.create 4096
let huffman = Z.N.dynamic_of_frequencies ~literals ~distances

let res = Buffer.create 16
type res = [ `Ok | `Partial | `Block ]

let pp_decode ppf = function
  | `Await -> Fmt.string ppf "`Await"
  | `End -> Fmt.string ppf "`End"
  | `Flush -> Fmt.string ppf "`Flush"
  | `Malformed err -> Fmt.pf ppf "(`Malformed %S)" err

let q0 = Z.B.of_list
    [ `Literal '\000'
    ; `Literal '\000'
    ; `Copy (1, 258)
    ; `Copy (1, 256)
    ; `End ]

let encoder = Z.N.encoder (`Buffer res) ~q:q0
let () = Fmt.epr "# dynamic huffman: done.\n%!"
let[@warning "-8"] `Ok : res = Z.N.encode encoder (`Block { kind= Z.N.Dynamic huffman; last= true })
let[@warning "-8"] `Ok : res = Z.N.encode encoder `Flush
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
  | `End -> Fmt.string ppf "`End"

let () = Z.B.reset q
let () = Fmt.pr "> @[<hov>%a@].\n%!" Fmt.(Dump.list pp_code) (Z.B.to_list q)

let source = "salut les copains, je compresse quelque chose!"
let state = Z.L.state (`String source) ~q ~w
let ret : Z.L.decode = Z.L.compress state

let () = Fmt.pr "Got %a.\n%!" pp_decode ret
let () = Fmt.pr "> @[<hov>%a@].\n%!" Fmt.(Dump.list pp_code) (Z.B.to_list q)

let reconstruct lst =
  let len = List.fold_left (fun a -> function `Literal _ -> 1 + a | `Copy (_, len) -> len + a | `End -> a) 0 lst in
  let res = Bytes.create len in
  let pos = ref 0 in
  List.iter (function
      | `Literal chr -> Bytes.set res !pos chr ; incr pos
      | `Copy (off, len) ->
        for _ = 0 to len - 1
        do Bytes.set res !pos (Bytes.get res (!pos - off)) ; incr pos done
      | `End -> ())
    lst ;
  Bytes.unsafe_to_string res

let () = Fmt.epr "source == reconstruct? %b.\n%!" (String.equal source (reconstruct (Z.B.to_list q)))
let () = Z.B.reset q

let compress ic oc =
  let state = Z.L.state (`Channel ic) ~w ~q in
  let encoder = Z.N.encoder (`Channel oc) ~q in
  let dynamic = ref Z.N.Fixed in

  let rec compress () = match Z.L.compress state with
    | `Await -> assert false
    | `End ->
      pending @@ Z.N.encode encoder (`Block { Z.N.kind= Z.N.Fixed; last= true; })
    | `Flush ->
      let lit = Z.L.literals state in
      let dst = Z.L.distances state in
      Fmt.epr "COMPRESS `FLUSH.\n%!" ;
      Fmt.epr "DUMP LIT: @[<hov>%a@].\n%!" Fmt.(Dump.array int) (lit :> int array) ;
      Fmt.epr "DUMP DST: @[<hov>%a@].\n%!" Fmt.(Dump.array int) (dst :> int array) ;
      dynamic := Z.N.Dynamic (Z.N.dynamic_of_frequencies ~literals:lit ~distances:dst) ;
      encode @@ Z.N.encode encoder (`Block { Z.N.kind= !dynamic; last= false; })
  and encode = function
    | `Partial -> assert false
    | `Ok -> compress ()
    | `Block ->
      Fmt.epr "ENCODE `BLOCK.\n%!" ;
      dynamic := Z.N.Dynamic
          (Z.N.dynamic_of_frequencies
             ~literals:(Z.L.literals state)
             ~distances:(Z.L.distances state)) ;
      encode @@ Z.N.encode encoder (`Block { Z.N.kind= !dynamic; last= false; })
  and pending = function
    | `Partial -> assert false
    | `Block -> assert false
    | `Ok ->
      Z.B.push_exn q Z.B.eob ;
      last @@ Z.N.encode encoder `Flush
  and last = function
    | `Ok -> ()
    | `Block -> assert false
    | `Partial -> assert false in

  compress ()

let oc = open_out "result.z"
let () = compress stdin oc ; close_out oc
