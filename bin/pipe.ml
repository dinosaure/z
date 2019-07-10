open Dd

let zlib_header = bigstring_create 2

let w = make_window ~bits:15
let o = bigstring_create io_buffer_size
let i = bigstring_create io_buffer_size
let q = B.create 4096

let run_inflate () =
  let decoder = M.decoder `Manual ~o ~w in
  let rec go () = match M.decode decoder with
    | `Await ->
      let len = Bs.bigstring_input Unix.stdin i 0 io_buffer_size in
      M.src decoder i 0 len ; go ()
    | `Flush ->
      let len = io_buffer_size - M.dst_rem decoder in
      Bs.bigstring_output Unix.stdout o 0 len ; M.flush decoder ; go ()
    | `Malformed err ->
      Fmt.epr "%s.\n%!" err ; `Error err
    | `End ->
      let len = io_buffer_size - M.dst_rem decoder in
      if len > 0 then Bs.bigstring_output Unix.stdout o 0 len ;
      `Ok () in
  go ()

let run_deflate () =
  let state = L.state `Manual ~w ~q in
  let kind = ref N.Fixed in
  let encoder = N.encoder `Manual ~q in

  N.dst encoder o 0 io_buffer_size ;

  let partial k encoder =
    let len = io_buffer_size - N.dst_rem encoder in
    let tmp = Bigstringaf.substring o ~off:0 ~len in
    if len > 0 then output_string stdout tmp ;
    N.dst encoder o 0 io_buffer_size ;
    k @@ N.encode encoder `Await in

  let rec compress () = match L.compress state with
    | `Await ->
      let len = Bs.bigstring_input Unix.stdin i 0 io_buffer_size in
      L.src state i 0 len ; compress ()
    | `End ->
      B.push_exn q B.eob ;
      pending @@ N.encode encoder (`Block { N.kind= N.Fixed; last= true; })
    | `Flush ->
      kind := N.Dynamic (N.dynamic_of_frequencies ~literals:(L.literals state) ~distances:(L.distances state)) ;
      encode @@ N.encode encoder (`Block { N.kind= !kind; last= false; })
  and encode = function
    | `Partial ->
      partial encode encoder
    | `Ok ->
      compress ()
    | `Block ->
      kind := N.Dynamic (N.dynamic_of_frequencies ~literals:(L.literals state) ~distances:(L.distances state)) ;
      encode @@ N.encode encoder (`Block { N.kind= !kind; last= false; })
  and pending = function
    | `Partial -> partial pending encoder
    | `Block -> assert false (* never occur! *)
    | `Ok -> last @@ N.encode encoder `Flush
  and last = function
    | `Partial -> partial last encoder
    | `Ok -> `Ok ()
    | `Block -> assert false in

  compress ()

let run = function
  | true -> run_deflate ()
  | false -> run_inflate ()

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
