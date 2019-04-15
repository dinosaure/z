let zlib_header = Z.bigstring_create 2

let w = Z.bigstring_create Z.Window.max
let o = Z.bigstring_create Z.io_buffer_size
let i = Z.bigstring_create Z.io_buffer_size

let run _ =
  let[@warning "-8"] 2 = Bs.bigstring_input Unix.stdin zlib_header 0 2 in
  let decoder = Z.M.decoder `Manual ~o ~w in
  let rec go () = match Z.M.decode decoder with
    | `Await ->
      let len = Bs.bigstring_input Unix.stdin i 0 Z.io_buffer_size in
      Z.M.src decoder i 0 len ; go ()
    | `Flush ->
      let len = Z.io_buffer_size - Z.M.dst_rem decoder in
      Bs.bigstring_output Unix.stdout o 0 len ; Z.M.flush decoder ; go ()
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
