open H

let load_filename filename =
  let ic = open_in (Fpath.to_string filename) in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ; close_in ic ;
  Bytes.unsafe_to_string rs

let diff source target =
  let source = load_filename source in
  let target = load_filename target in
  let source =
    Bigstringaf.of_string ~off:0 ~len:(String.length source) source in
  let target =
    Bigstringaf.of_string ~off:0 ~len:(String.length target) target in
  let encoder = N.encoder (`Channel stdout)
      ~src_len:(Bigstringaf.length source)
      ~dst_len:(Bigstringaf.length target) in
  let index = Duff.make source in
  let delta = Duff.delta index target in
  List.iter
    (function
      | Duff.Copy (off, len) ->
        let[@warning "-8"] `Ok : [ `Ok | `Partial ] =
          N.encode encoder (`Copy (off, len)) in ()
      | Duff.Insert (off, len) ->
        let slice = Bigstringaf.substring target ~off ~len in
        let[@warning "-8"] `Ok : [ `Ok | `Partial ] =
          N.encode encoder (`Insert slice) in ())
    delta ;
  match N.encode encoder `End with
  | `Ok -> ()
  | `Partial -> assert false

let patch source patch =
  let source = load_filename source in
  let source =
    Bigstringaf.of_string ~off:0 ~len:(String.length source) source in
  let ic = match patch with
    | `Stdin -> stdin
    | `Filename filename -> open_in (Fpath.to_string filename) in
  let dst = ref bigstring_empty in
  let decoder = M.decoder ~source (`Channel ic) in
  let rec go () = match M.decode decoder with
    | `Await -> assert false
    | `Destination len ->
      dst := Bigstringaf.create len ;
      M.dst decoder !dst 0 len ; go ()
    | `End ->
      assert (M.dst_rem decoder = 0) ;
      if patch <> `Stdin then close_in ic ;
      Bs.bigstring_output Unix.stdout !dst 0 (Bigstringaf.length !dst)
    | `Malformed err -> failwith err in
  go ()

open Cmdliner

let existing_file =
  let parser x = match Fpath.of_string x with
    | Ok _ as v when Sys.file_exists x -> v
    | Ok v -> Rresult.R.error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<file>" (parser, pp)

let existing_file_or_stdin =
  let parser = function
    | "-" -> Ok `Stdin
    | x -> match Fpath.of_string x with
      | Ok v when Sys.file_exists x -> Ok (`Filename v)
      | Ok v -> Rresult.R.error_msgf "%a does not exist" Fpath.pp v
      | Error _ as err -> err in
  let pp ppf = function
    | `Stdin -> Fmt.string ppf "#stdin"
    | `Filename v -> Fpath.pp ppf v in
  Arg.conv ~docv:"<file-or-stdin>" (parser, pp)

let source =
  let doc = "Source file" in
  Arg.(required & pos 0 (some existing_file) None & info [] ~doc)

let target =
  let doc = "Target file" in
  Arg.(required & pos 1 (some existing_file) None & info [] ~doc)

let p =
  let doc = "Patch file" in
  Arg.(value & pos 1 existing_file_or_stdin `Stdin & info [] ~doc)

let diff_cmd =
  let doc = "Diff tool." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Generate a diff file which is a thin representation of \
          $(i,TARGET) from $(i,SOURCE)" ] in
  Term.(const diff $ source $ target),
  Term.info "diff" ~doc ~exits ~man

let patch_cmd =
  let doc = "Patch tool" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Reconstruct a diff input from $(i,SOURCE)" ] in
  Term.(const patch $ source $ p),
  Term.info "patch" ~doc ~exits ~man

let main = `Help (`Pager, None)

let cmd =
  let doc = "Diff tool" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Diff tool, libXdiff algorithm in OCaml" ] in
  Term.(ret (const main)),
  Term.info "diff" ~doc ~exits ~man

let () = Term.(exit @@ eval_choice cmd [ diff_cmd; patch_cmd ])
