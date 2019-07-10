open Dd

let literals = make_literals ()
let () = succ_literal literals '\000'
let () = succ_literal literals '\000'
let () = succ_length literals 258
let () = succ_length literals 256
let distances = make_distances ()
let () = succ_distance distances 1
let () = succ_distance distances 1

let w = make_window ~bits:15
let o = bigstring_create io_buffer_size
let q = B.create 4096
let huffman = N.dynamic_of_frequencies ~literals ~distances

let res = Buffer.create 16
type res = [ `Ok | `Partial | `Block ]

let pp_decode ppf = function
  | `Await -> Fmt.string ppf "`Await"
  | `End -> Fmt.string ppf "`End"
  | `Flush -> Fmt.string ppf "`Flush"
  | `Malformed err -> Fmt.pf ppf "(`Malformed %S)" err

let q0 = B.of_list
    [ `Literal '\000'
    ; `Literal '\000'
    ; `Copy (1, 258)
    ; `Copy (1, 256)
    ; `End ]

let encoder = N.encoder (`Buffer res) ~q:q0
let () = Fmt.epr "# dynamic huffman: done.\n%!"
let[@warning "-8"] `Ok : res = N.encode encoder (`Block { kind= N.Dynamic huffman; last= true })
let[@warning "-8"] `Ok : res = N.encode encoder `Flush
let () = Fmt.pr "deflated: @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.O.default) (Buffer.contents res)
let () = Fmt.pr "deflated: %S\n%!" (Buffer.contents res)

(* We should have: "\xed\xc0\x01\x01\x00\x00\x00\x40\x20\xff\x57\x1b\x42\x2c\x4f" *)

let decoder = M.decoder (`String (Buffer.contents res)) ~o ~w
let res = M.decode decoder
let () = Fmt.pr "> %a.\n%!" pp_decode res
let () =
  if res = `End
  then let len = (io_buffer_size - M.dst_rem decoder) in
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

let () = B.reset q
let () = Fmt.pr "> @[<hov>%a@].\n%!" Fmt.(Dump.list pp_code) (B.to_list q)

let source = "salut les copains, je compresse quelque chose!"
let state = L.state (`String source) ~q ~w
let ret : L.decode = L.compress state

let () = Fmt.pr "Got %a.\n%!" pp_decode ret
let () = Fmt.pr "> @[<hov>%a@].\n%!" Fmt.(Dump.list pp_code) (B.to_list q)

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

let () = Fmt.epr "source == reconstruct? %b.\n%!" (String.equal source (reconstruct (B.to_list q)))
let () = B.reset q

let compress ic oc =
  let state = L.state (`Channel ic) ~w ~q in
  let encoder = N.encoder (`Channel oc) ~q in
  let dynamic = ref N.Fixed in

  let rec compress () = match L.compress state with
    | `Await -> assert false
    | `End ->
      pending @@ N.encode encoder (`Block { N.kind= N.Fixed; last= true; })
    | `Flush ->
      let lit = L.literals state in
      let dst = L.distances state in
      Fmt.epr "COMPRESS `FLUSH.\n%!" ;
      Fmt.epr "DUMP LIT: @[<hov>%a@].\n%!" Fmt.(Dump.array int) (lit :> int array) ;
      Fmt.epr "DUMP DST: @[<hov>%a@].\n%!" Fmt.(Dump.array int) (dst :> int array) ;
      dynamic := N.Dynamic (N.dynamic_of_frequencies ~literals:lit ~distances:dst) ;
      encode @@ N.encode encoder (`Block { N.kind= !dynamic; last= false; })
  and encode = function
    | `Partial -> assert false
    | `Ok -> compress ()
    | `Block ->
      Fmt.epr "ENCODE `BLOCK.\n%!" ;
      dynamic := N.Dynamic
          (N.dynamic_of_frequencies
             ~literals:(L.literals state)
             ~distances:(L.distances state)) ;
      encode @@ N.encode encoder (`Block { N.kind= !dynamic; last= false; })
  and pending = function
    | `Partial -> assert false
    | `Block -> assert false
    | `Ok ->
      B.push_exn q B.eob ;
      last @@ N.encode encoder `Flush
  and last = function
    | `Ok -> ()
    | `Block -> assert false
    | `Partial -> assert false in

  compress ()

let oc = open_out "result.z"
let () = compress stdin oc ; close_out oc
