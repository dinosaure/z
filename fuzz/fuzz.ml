let w = Z.bigstring_create Z.Window.max
let o = Z.bigstring_create Z.io_buffer_size

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
           Fmt.pf ppf "byte: @[<hov%a@]\n%!" (Hxd_string.pp Hxd.O.default) bytes
         ; Fmt.pf ppf "res0: @[<hov%a@]\n%!" (Hxd_string.pp Hxd.O.default) res0
         ; Fmt.pf ppf "res1: @[<hov%a@]\n%!" (Hxd_string.pp Hxd.O.default) res1 ) ;

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
  let expected = apply_cmds cmds in
  let buf = Buffer.create 16 in
  let literals, distances = frequencies_of_cmds cmds in
  let dynamic = Z.N.dynamic_of_frequencies ~literals ~distances in
  let encoder = Z.N.encoder ~last:true (`Buffer buf) (Z.N.Dynamic dynamic) in
  List.iter (fun v -> match Z.N.encode encoder v with
      | `Ok -> ()
      | `Partial -> Crowbar.fail "Impossible `Partial case")
    ((cmds :> Z.N.encode list) @ [ `End ]) ;
  let bytes = Buffer.contents buf in
  let res0 = zlib bytes in
  let res1 = z bytes in

  Fmt.epr "deflated: %S.\n%!" bytes ;
  Fmt.epr "deflated: @[<hov>%a@].\n%!" Fmt.(Dump.list pp_cmd) cmds ;
  Fmt.epr "inflated (zlib): %S.\n%!" res0 ;
  Fmt.epr "inflated (z): %S.\n%!" res1 ;
  Fmt.epr "inflated (fuzz): %S.\n%!" expected ;

  Crowbar.check_eq ~pp:pp_string ~eq:String.equal expected res0 ;
  Crowbar.check_eq ~pp:pp_string ~eq:String.equal res0 res1
