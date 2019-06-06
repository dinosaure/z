let w = Z.bigstring_create Z.Window.max
let o = Z.bigstring_create Z.io_buffer_size
let q = Z.B.create 4096

exception End_of_input

let zlib bytes =
  let off = ref 0 in
  let buf = Buffer.create 16 in
  try
    Zlib.uncompress ~header:false
      (fun ibuf ->
         if String.length bytes - !off = 0 then raise End_of_input ;
         let len = min (Bytes.length ibuf) (String.length bytes - !off) in
         Bytes.blit_string bytes !off ibuf 0 len ;
         off := !off + len ; len)
      (fun obuf len ->
         Buffer.add_subbytes buf obuf 0 len) ;
    Buffer.contents buf
  with Zlib.Error _ -> Crowbar.bad_test ()
     | End_of_input -> Crowbar.bad_test ()

let z bytes =
  let decoder = Z.M.decoder (`String bytes) ~o ~w in
  let buf = Buffer.create 16 in

  let rec go () = match Z.M.decode decoder with
    | `Await -> assert false
    | `Flush ->
      let len = Z.io_buffer_size - (Z.M.dst_rem decoder) in
      let res = Bigstringaf.substring o ~off:0 ~len in
      Buffer.add_string buf res ;
      Z.M.flush decoder ;
      go ()
    | `End ->
      let len = Z.io_buffer_size - (Z.M.dst_rem decoder) in
      let res = Bigstringaf.substring o ~off:0 ~len in
      Buffer.add_string buf res ;
      Z.M.flush decoder ;
      Buffer.contents buf
    | `Malformed err -> Crowbar.fail err in
  go ()

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

let uniq =
  let v = ref (-1) in
  fun () -> incr v ; !v

let () =
  Crowbar.add_test ~name:"z/zlib" [ Crowbar.bytes ] @@ fun bytes ->
  Fmt.epr "Test %S.\n%!" bytes ;
  let res0 = zlib bytes in
  Fmt.epr "Process %S.\n%!" bytes ;
  let res1 = z bytes in
  Fmt.epr "Check %S.\n%!" bytes ;

  if String.equal res0 res1 = false
  then ( let id = uniq () in
         let oc = open_out (Fmt.strf "fuzz-%d" id) in
         let ppf = Format.formatter_of_out_channel oc in
           Fmt.pf ppf "byte: @[<hov%a@]\n%!" pp_string bytes
         ; Fmt.pf ppf "res0: @[<hov%a@]\n%!" pp_string res0
         ; Fmt.pf ppf "res1: @[<hov%a@]\n%!" pp_string res1 ) ;

  Crowbar.check_eq ~pp:pp_string ~eq:String.equal res0 res1

let literal chr = `Literal chr
let ( <.> ) f g = fun x -> f (g x)

let gen_cmd = Crowbar.choose
    [ Crowbar.(map [ range 256 ] (literal <.> Char.chr))
    ; Crowbar.(map [ range 256; range 32768 ] (fun len off -> `Copy (1 + off, 3 + len))) ]

let frequencies_of_cmds cmds =
  let literals = Z.N.make_literals () in
  let distances = Z.N.make_distances () in
  List.iter
    (function
      | `Literal chr ->
        Z.N.succ_literal literals chr
      | `Copy (off, len) ->
        Z.N.succ_length literals len ;
        Z.N.succ_distance distances off)
    cmds ;
  literals, distances

let check_cmds cmds =
  let exception Bad in
  let write = ref 0 in
  try List.iter
        (function
          | `Literal _ -> incr write
          | `Copy (off, len) -> if !write - off < 0 then raise_notrace Bad ; write := !write + len)
        cmds ; true
  with Bad -> false

let apply_cmds cmds =
  let buf = Buffer.create 16 in
  List.iter
    (function
      | `Literal chr -> Buffer.add_char buf chr
      | `Copy (off, len) ->
        for _ = 0 to len - 1 do Buffer.add_char buf (Buffer.nth buf (Buffer.length buf - off)) done)
    cmds ;
  Buffer.contents buf

let zlib bytes =
  let off = ref 0 in
  let buf = Buffer.create 16 in
  try
    Zlib.uncompress ~header:false
      (fun ibuf ->
         if String.length bytes - !off = 0 then raise End_of_input ;
         let len = min (Bytes.length ibuf) (String.length bytes - !off) in
         Bytes.blit_string bytes !off ibuf 0 len ;
         off := !off + len ; len)
      (fun obuf len ->
         Buffer.add_subbytes buf obuf 0 len) ;
    Buffer.contents buf
  with Zlib.Error _ -> Crowbar.bad_test ()
     | End_of_input -> Crowbar.bad_test ()

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> String.make 1 x | chr -> Fmt.strf "\\%03d" (Char.code chr)) Fmt.string

let pp_cmd ppf = function
  | `Literal chr -> Fmt.pf ppf "(`Literal %a)" pp_chr chr
  | `Copy (off, len) -> Fmt.pf ppf "(`Copy off:%d, len:%d)" off len

let () =
  Crowbar.add_test ~name:"z/zlib" [ Crowbar.list gen_cmd ] @@ fun cmds ->
  if not (check_cmds cmds) then Crowbar.bad_test () ;
  Z.B.reset q ;

  Fmt.epr "deflated: @[<hov>%a@].\n%!" Fmt.(Dump.list pp_cmd) cmds ;

  let expected = apply_cmds cmds in
  let buf = Buffer.create 16 in
  let literals, distances = frequencies_of_cmds cmds in
  let dynamic = Z.N.dynamic_of_frequencies ~literals ~distances in
  let encoder = Z.N.encoder (`Buffer buf) { kind= Z.N.Dynamic dynamic; last= true; } ~q in
  List.iter (fun v -> match Z.N.encode encoder v with
      | `Ok -> ()
      | `End _ -> Crowbar.fail "Impossible `End case"
      | `Partial -> Crowbar.fail "Impossible `Partial case")
    ((cmds :> Z.N.encode list) @ [ `End ]) ;
  let bytes = Buffer.contents buf in
  let res0 = zlib bytes in
  let res1 = z bytes in

  Fmt.epr "deflated: %S.\n%!" bytes ;
  Fmt.epr "inflated (zlib): %S.\n%!" res0 ;
  Fmt.epr "inflated (z): %S.\n%!" res1 ;
  Fmt.epr "inflated (fuzz): %S.\n%!" expected ;

  Crowbar.check_eq ~pp:pp_string ~eq:String.equal expected res0 ;
  Crowbar.check_eq ~pp:pp_string ~eq:String.equal res0 res1

let ( <.> ) f g = fun x -> f (g x)

let non_empty_bytes n : string Crowbar.gen =
  let open Crowbar in
  let ( >>= ) = dynamic_bind in

  let rec go acc = function
    | 0 -> concat_gen_list (const "") acc
    | n -> go (map [ uint8 ] (String.make 1 <.> Char.chr) :: acc) (pred n) in
  let gen n = go [] n in

  range n >>= (gen <.> succ)

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

let pp_code ppf = function
  | `Literal chr -> Fmt.pf ppf "(`Literal %02x:%a)" (Char.code chr) pp_chr chr
  | `Copy (off, len) -> Fmt.pf ppf "(`Copy off:%d len:%d)" off len

let () =
  Crowbar.add_test ~name:"lz77" [ Crowbar.list (non_empty_bytes 1024) ] @@ fun inputs ->
  Z.B.reset q ;
  let state = Z.L.state `Manual ~w ~q in
  let res = ref [] in
  let rec go inputs = match Z.L.compress state with
    | `End ->
      let lst = Z.B.to_list q in
      res := lst :: !res ;
      Z.B.junk_exn q (List.length lst) ;
      List.rev !res
    | `Flush ->
      let lst = Z.B.to_list q in
      res := lst :: !res ;
      Z.B.junk_exn q (List.length lst) ;
      go inputs
    | `Await -> match inputs with
      | [] -> Z.L.src state Bigstringaf.empty 0 0 ; go []
      | x :: r ->
        let x = Bigstringaf.of_string x ~off:0 ~len:(String.length x) in
        Z.L.src state x 0 (Bigstringaf.length x) ; go r in
  let res = go inputs in
  let res = List.concat res in

  Fmt.epr "Results: @[<hov>%a@].\n%!" Fmt.(Dump.list pp_code) res ;

  Crowbar.check_eq ~pp:pp_string ~eq:String.equal ~cmp:String.compare (String.concat "" inputs) (reconstruct res)
