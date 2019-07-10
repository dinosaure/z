open Dd

let w = make_window ~bits:15
let i = bigstring_create io_buffer_size
let o = bigstring_create io_buffer_size
let q = B.create 4096

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
  let decoder = M.decoder (`String bytes) ~o ~w in
  let buf = Buffer.create 16 in

  let rec go () = match M.decode decoder with
    | `Await -> assert false
    | `Flush ->
      let len = io_buffer_size - (M.dst_rem decoder) in
      let res = Bigstringaf.substring o ~off:0 ~len in
      Buffer.add_string buf res ;
      M.flush decoder ;
      go ()
    | `End ->
      let len = io_buffer_size - (M.dst_rem decoder) in
      let res = Bigstringaf.substring o ~off:0 ~len in
      Buffer.add_string buf res ;
      M.flush decoder ;
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
  let res0 = zlib bytes in
  let res1 = z bytes in

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
  let literals = make_literals () in
  let distances = make_distances () in
  List.iter
    (function
      | `Literal chr ->
        succ_literal literals chr
      | `Copy (off, len) ->
        succ_length literals len ;
        succ_distance distances off)
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

type cmd = [ `Literal of char | `Copy of int * int | `End ]

let () =
  Crowbar.add_test ~name:"z/zlib" [ Crowbar.list gen_cmd ] @@ fun cmds ->
  if not (check_cmds cmds) then Crowbar.bad_test () ;
  B.reset q ;

  List.iter (B.push_exn q <.> B.cmd) (cmds :> cmd list) ;
  B.push_exn q B.eob ;

  let expected = apply_cmds cmds in
  let buf = Buffer.create 16 in
  let literals, distances = frequencies_of_cmds cmds in
  let dynamic = N.dynamic_of_frequencies ~literals ~distances in
  let encoder = N.encoder (`Buffer buf) ~q in
  List.iter (fun v -> match N.encode encoder v with
      | `Ok -> ()
      | `Block -> Crowbar.fail "Impossible `Block case"
      | `Partial -> Crowbar.fail "Impossible `Partial case")
    [ `Block { N.kind= N.Dynamic dynamic; last= true; }; `Flush ] ;
  let bytes = Buffer.contents buf in
  let res0 = zlib bytes in
  let res1 = z bytes in

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
  let len = List.fold_left (fun a -> function `Literal _ -> 1 + a | `Copy (_, len) -> len + a | `End -> a) 0 lst in
  let res = Bytes.create len in
  let pos = ref 0 in
  List.iter (function
      | `End -> ()
      | `Literal chr -> Bytes.set res !pos chr ; incr pos
      | `Copy (off, len) ->
        for _ = 0 to len - 1
        do Bytes.set res !pos (Bytes.get res (!pos - off)) ; incr pos done)
    lst ;
  Bytes.unsafe_to_string res

let pp_code ppf = function
  | `Literal chr -> Fmt.pf ppf "(`Literal %02x:%a)" (Char.code chr) pp_chr chr
  | `Copy (off, len) -> Fmt.pf ppf "(`Copy off:%d len:%d)" off len
  | `End -> Fmt.pf ppf "`End"

let () =
  Crowbar.add_test ~name:"lz77" [ Crowbar.list (non_empty_bytes 1024) ] @@ fun inputs ->
  B.reset q ;
  let state = L.state `Manual ~w ~q in
  let res = ref [] in
  let rec go inputs = match L.compress state with
    | `End ->
      let lst = B.to_list q in
      res := lst :: !res ;
      B.junk_exn q (List.length lst) ;
      List.rev !res
    | `Flush ->
      let lst = B.to_list q in
      res := lst :: !res ;
      B.junk_exn q (List.length lst) ;
      go inputs
    | `Await -> match inputs with
      | [] -> L.src state Bigstringaf.empty 0 0 ; go []
      | x :: r ->
        let x = Bigstringaf.of_string x ~off:0 ~len:(String.length x) in
        L.src state x 0 (Bigstringaf.length x) ; go r in
  let res = go inputs in
  let res = List.concat res in

  Crowbar.check_eq ~pp:pp_string ~eq:String.equal ~cmp:String.compare (String.concat "" inputs) (reconstruct res)

let split payload =
  let res = ref [] in
  let tmp = Bytes.create 1024 in
  let rec go consumed pos =
    if consumed + pos = String.length payload
    then ( if pos = 0 then List.rev !res else List.rev (Bytes.sub_string tmp 0 pos :: !res) )
    else if pos = 1024
    then ( res := Bytes.to_string tmp :: !res ; go (consumed + 1024) 0 )
    else ( Bytes.set tmp pos payload.[consumed + pos]
         ; go consumed (succ pos) ) in
  go 0 0

let () =
  Crowbar.add_test ~name:"compress/uncompress" [ Crowbar.list (non_empty_bytes 1024) ] @@ fun inputs ->
  B.reset q ;
  let res = Buffer.create 4096 in
  let payloads = ref inputs in

  let flush o len = for i = 0 to len - 1 do Buffer.add_char res o.{i} done in
  let refill i = match !payloads with
    | [] -> 0
    | data :: rest ->
      for x = 0 to String.length data - 1 do i.{x} <- data.[x] done ;
      payloads := rest ; String.length data in
  Higher.compress ~w ~q ~i ~o ~refill ~flush ;

  let splits = split (Buffer.contents res) in
  Buffer.clear res ; payloads := splits ;

  let flush o len = for i = 0 to len - 1 do Buffer.add_char res o.{i} done in
  let refill i = match !payloads with
    | [] -> 0
    | data :: rest ->
      for x = 0 to String.length data - 1 do i.{x} <- data.[x] done ;
      payloads := rest ; String.length data in

  Higher.decompress ~w ~i ~o ~refill ~flush ;

  Crowbar.check_eq ~eq:String.equal ~pp:pp_string ~cmp:String.compare
    (Buffer.contents res) (String.concat "" inputs)

let q = B.create 0xffff

let () =
  Crowbar.add_test ~name:"flat compression" [ Crowbar.list (non_empty_bytes 0xffff) ] @@ fun inputs ->
  let res = Buffer.create 4096 in
  let payloads = ref inputs in
  let encoder = N.encoder (`Buffer res) ~q in

  let fill q s =
    for i = 0 to String.length s - 1 do B.push_exn q (B.literal s.[i]) done in

  let rec go = function
    | `Ok -> Buffer.contents res
    | `Partial -> assert false
    | `Block -> match !payloads with
      | [] -> assert false (* XXX(dinosaure): or [{ kind= Flat 0; last= true; }] *)
      | [ x ] ->
        fill q x ; payloads := [] ;
        go (N.encode encoder (`Block { N.kind= N.Flat (String.length x); last= true; }))
      | x :: r ->
        fill q x ; payloads := r ;
        go (N.encode encoder (`Block { N.kind= N.Flat (String.length x); last= false; })) in
  B.push_exn q B.eob ;
  let res0 = go (N.encode encoder `Flush) in
  let buf1 = Buffer.create 4096 in

  let flush b l = for i = 0 to l - 1 do Buffer.add_char buf1 b.{i} done in
  Higher.of_string ~o ~w res0 ~flush ;

  Crowbar.check_eq ~eq:String.equal ~pp:pp_string ~cmp:String.compare
    (Buffer.contents buf1) (String.concat "" inputs)

