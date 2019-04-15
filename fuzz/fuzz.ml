let w = Z.bigstring_create Z.Window.max
let o = Z.bigstring_create Z.io_buffer_size

let zlib bytes =
  let off = ref 0 in
  let buf = Buffer.create 16 in
  try
    Zlib.uncompress ~header:false
      (fun ibuf ->
         let len = min (Bytes.length ibuf) (String.length bytes - !off) in
         Bytes.blit_string bytes !off ibuf 0 len ;
         off := !off + len ; len)
      (fun obuf len ->
         Buffer.add_subbytes buf obuf 0 len) ;
    Buffer.contents buf
  with Zlib.Error _ -> Crowbar.bad_test ()

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

let () =
  Crowbar.add_test ~name:"z/zlib" [ Crowbar.bytes ] @@ fun bytes ->
  Fmt.epr "Test %S.\n%!" bytes ;
  let res0 = zlib bytes in
  Fmt.epr "Process %S.\n%!" bytes ;
  let res1 = z bytes in
  Fmt.epr "Check %S.\n%!" bytes ;
  Crowbar.check_eq ~pp:pp_string ~eq:String.equal res0 res1
