let zlib_header = Bytes.create 2

let run _ =
  let[@warning "-8"] 2 = input stdin zlib_header 0 2 in
  let decoder = Z.M.decoder `Manual 0x800 in
  let raw = Bytes.create 0x800 in
  let rec go () = match Z.M.decode decoder with
    | `Await ->
      let len = input stdin raw 0 (Bytes.length raw) in
      Z.M.src decoder raw 0 len ; go ()
    | `Flush _o ->
      let _len = 0x800 - Z.M.dst_rem decoder in
      (* output stdout o 0 len ; *) Z.M.flush decoder ; go ()
    | `Continuation -> go ()
    | `Malformed err ->
      Fmt.epr "%s.\n%!" err ; `Error err
    | `End -> `Ok () in
  go ()

open Cmdliner

let deflate =
  let doc = "Deflate input." in
  Arg.(value & flag & info [ "d" ] ~doc)

let command =
  let doc = "Pipe." in
  let exits = Term.default_exits in
  let man =
    [ `S "Description"
    ; `P "$(tname) takes a standard input and write in standard output the compressed/uncompressed data." ] in
  Term.(pure run $ deflate),
  Term.info "pipe" ~exits ~doc ~man

let () = Term.(exit @@ eval command)
