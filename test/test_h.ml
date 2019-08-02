open H

let o = bigstring_create io_buffer_size

let test_0 () =
  Alcotest.test_case "empty" `Quick @@ fun () ->
  let encoder = N.encoder `Manual ~src_len:0 ~dst_len:0 in
  let buf = Buffer.create 16 in
  N.dst encoder o 0 io_buffer_size ;
  let rec go = function
    | `Ok -> Buffer.contents buf
    | `Partial ->
      let len = io_buffer_size - N.dst_rem encoder in
      let str = Bigstringaf.substring o ~off:0 ~len in
      Buffer.add_string buf str ; go (N.encode encoder `Await) in
  let res = go (N.encode encoder `End) in
  Alcotest.(check string) "empty diff" res "\000\000" ;
  let decoder = M.decoder ~source:bigstring_empty (`String res) in
  let rec go () = match M.decode decoder with
    | `Destination len ->
      M.dst decoder bigstring_empty 0 0 ;
      Alcotest.(check int) "dst len" 0 len ; go ()
    | `End -> ()
    | `Await -> Fmt.failwith "`Await"
    | `Malformed err -> failwith err in
  go ()

let test_1 () =
  Alcotest.test_case "only insert" `Quick @@ fun () ->
  let buf = Buffer.create 16 in
  let encoder = N.encoder (`Buffer buf) ~src_len:0 ~dst_len:4 in
  let f e = match N.encode encoder e with
    | `Ok -> () | `Partial -> assert false in
  List.iter f [ `Insert "aaaa"; `End ] ;
  let res = Buffer.contents buf in
  Alcotest.(check string) "insert diff" res "\000\004\004aaaa" ;
  let decoder = M.decoder ~source:bigstring_empty (`String res) in
  let dst = ref bigstring_empty in
  let rec go () = match M.decode decoder with
    | `Destination len ->
      Alcotest.(check int) "dst len" 4 len ;
      dst := Bigstringaf.create len ;
      M.dst decoder !dst 0 len ; go ()
    | `End -> Bigstringaf.to_string !dst
    | `Await -> Fmt.failwith "`Await"
    | `Malformed err -> failwith err in
  let res = go () in
  Alcotest.(check string) "insert diff" res "aaaa"

let test_2 () =
  Alcotest.test_case "only copy" `Quick @@ fun () ->
  let buf = Buffer.create 16 in
  let encoder = N.encoder (`Buffer buf) ~src_len:4 ~dst_len:4 in
  let f e = match N.encode encoder e with
    | `Ok -> () | `Partial -> assert false in
  List.iter f [ `Copy (0, 4); `End ] ;
  let res = Buffer.contents buf in
  Alcotest.(check string) "copy diff" res "\004\004\x90\004" ;
  let decoder = M.decoder ~source:(Bigstringaf.of_string ~off:0 ~len:4 "aaaa") (`String res) in
  let dst = ref bigstring_empty in
  let rec go () = match M.decode decoder with
    | `Destination len ->
      Alcotest.(check int) "dst len" 4 len ;
      dst := Bigstringaf.create len ;
      M.dst decoder !dst 0 len ; go ()
    | `End -> Bigstringaf.to_string !dst
    | `Await -> Fmt.failwith "`Await"
    | `Malformed err -> failwith err in
  let res = go () in
  Alcotest.(check string) "copy diff" res "aaaa"

let () =
  Alcotest.run "h"
    [ "valids", [ test_0 ()
                ; test_1 ()
                ; test_2 () ] ]
