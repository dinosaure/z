let zlib_header = Z.bigstring_create 2

let w = Z.bigstring_create Z.Window.max
let o = Z.bigstring_create Z.io_buffer_size
let i = Z.bigstring_create Z.io_buffer_size
let q = Z.B.create 4096

let run_inflate () =
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
    | `End ->
      let len = Z.io_buffer_size - Z.M.dst_rem decoder in
      if len > 0 then Bs.bigstring_output Unix.stdout o 0 len ;
      `Ok () in
  go ()

let run_deflate () =
  let state = Z.L.state `Manual ~w ~q in
  let kind = ref Z.N.Fixed in
  let encoder = Z.N.encoder `Manual ~q in

  Z.N.dst encoder o 0 Z.io_buffer_size ;

  let partial k encoder =
    let len = Z.io_buffer_size - Z.N.dst_rem encoder in
    let tmp = Bigstringaf.substring o ~off:0 ~len in
    if len > 0 then output_string stdout tmp ;
    Z.N.dst encoder o 0 Z.io_buffer_size ;
    k @@ Z.N.encode encoder `Await in

  let rec compress () = match Z.L.compress state with
    | `Await ->
      let len = Bs.bigstring_input Unix.stdin i 0 Z.io_buffer_size in
      Z.L.src state i 0 len ; compress ()
    | `End ->
      Z.B.push_exn q Z.B.eob ;
      pending @@ Z.N.encode encoder (`Block { Z.N.kind= Z.N.Fixed; last= true; })
    | `Flush ->
      Fmt.epr ">>>> need to flush queue.\n%!" ;
      kind := Z.N.Dynamic (Z.N.dynamic_of_frequencies ~literals:(Z.L.literals state) ~distances:(Z.L.distances state)) ;
      encode @@ Z.N.encode encoder (`Block { Z.N.kind= !kind; last= false; })
  and encode = function
    | `Partial ->
      Fmt.epr ">>>> output (dst_rem: %d).\n%!" (Z.N.dst_rem encoder) ;
      partial encode encoder
    | `Ok ->
      Fmt.epr "encoding: done (dst_rem: %d).\n%!" (Z.N.dst_rem encoder) ;
      compress ()
    | `Block ->
      Fmt.epr ">>>> reload huffman.\n%!" ;
      kind := Z.N.Dynamic (Z.N.dynamic_of_frequencies ~literals:(Z.L.literals state) ~distances:(Z.L.distances state)) ;
      encode @@ Z.N.encode encoder (`Block { Z.N.kind= !kind; last= false; })
  and pending = function
    | `Partial -> partial pending encoder
    | `Block -> assert false (* never occur! *)
    | `Ok -> last @@ Z.N.encode encoder `Flush
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
