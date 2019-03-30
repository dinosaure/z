let w = Z.bigstring_create Z.Window.max
let o = Z.bigstring_create Z.io_buffer_size

let decode =
  let pp ppf = function
    | `Await -> Fmt.string ppf "`Await"
    | `Flush -> Fmt.string ppf "`Flush"
    | `End -> Fmt.string ppf "`End"
    | `Malformed err -> Fmt.pf ppf "(`Malformed %s)" err in
  let equal a b = match a, b with
    | `Await, `Await -> true
    | `Flush, `Flush -> true
    | `End, `End -> true
    | `Malformed a, `Malformed b -> String.equal a b
    | _, _ -> false in
  Alcotest.testable pp equal

let invalid_complement_of_length () =
  Alcotest.test_case "invalid complement of length" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x00\x00\x00\x00\x00") ~o ~w in
  Alcotest.(check decode) "invalid complement of length"
    (Z.M.decode decoder) (`Malformed "Invalid complement of length")

let invalid_kind_of_block () =
  Alcotest.test_case "invalid kind of block" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x06") ~o ~w in
  Alcotest.(check decode) "invalid kind of block"
    (Z.M.decode decoder) (`Malformed "Invalid kind of block")

let invalid_code_lengths () =
  Alcotest.test_case "invalid code lengths" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x04\x00\xfe\xff") ~o ~w in
  Alcotest.(check decode) "invalid code lengths"
    (Z.M.decode decoder) (`Malformed "Invalid dictionary")

let invalid_bit_length_repeat () =
  Alcotest.test_case "invalid bit length repeat" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x04\x00\x24\x49\x00") ~o ~w in
  Alcotest.(check decode) "invalid bit length repeat"
    (Z.M.decode decoder) (`Malformed "Unexpected end of input")
(* XXX(dinosaure): error is not conform to what we expect (best will be [Invalid
   dictionary]), TODO! *)

let invalid_codes () =
  Alcotest.test_case "invalid codes -- missing end-of-block" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x04\x00\x24\xe9\xff\x6d") ~o ~w in
  Alcotest.(check decode) "invalid codes -- missing end-of-block"
    (Z.M.decode decoder) (`Malformed "Invalid dictionary")

let invalid_lengths () =
  Alcotest.test_case "invalid literals/lengths" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x04\x80\x49\x92\x24\x49\x92\x24\x49\x92\x24\x71\xff\xff\x93\x11\x00") ~o ~w in
  Alcotest.(check decode) "invalid literals/lengths"
    (Z.M.decode decoder) (`Malformed "Invalid dictionary")

let invalid_distances () =
  Alcotest.test_case "invalid distances" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x04\x80\x49\x92\x24\x49\x92\x24\x0f\xb4\xff\xff\xc3\x84") ~o ~w in
  Alcotest.(check decode) "invalid distances"
    (Z.M.decode decoder) (`Malformed "Invalid dictionary")

let too_many_length_or_distance_symbols () =
  Alcotest.test_case "too many length of distance symbols" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\xfc\x00\x00") ~o ~w in
  Alcotest.(check decode) "too many length of distance symbols"
    (Z.M.decode decoder) (`Malformed "Unexpected end of input")
(* XXX(dinosaure): error is not conform to what we expect (best will be [Invalid
   dictionary]), TODO! *)

let invalid_distance_code () =
  Alcotest.test_case "invalid distance code" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x02\x7e\xff\xff") ~o ~w in
  Alcotest.(check decode) "invalid distance code"
    (Z.M.decode decoder) `Flush ;
  Fmt.epr "dst_rem: %d.\n%!" (Z.M.dst_rem decoder) ;
  Alcotest.(check string) "non-corrupted output"
    "" (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))
(* XXX(dinosaure): see [Z.M.base_dist]'s comment about this behavior. *)

let invalid_distance_too_far_back () =
  Alcotest.test_case "invalid distance too far back" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x0c\xc0\x81\x00\x00\x00\x00\x00\x90\xff\x6b\x04\x00") ~o ~w in
  Alcotest.(check decode) "invalid distance too far back"
    (Z.M.decode decoder) `Flush
(* XXX(dinosaure): see [Invalid_distance] about this kind of input. *)

let fixed () =
  Alcotest.test_case "fixed" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x03\x00") ~o ~w in
  Alcotest.(check decode) "fixed"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Alcotest.(check string) "empty"
    "" (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let stored () =
  Alcotest.test_case "stored" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x01\x01\x00\xfe\xff\x00") ~o ~w in
  Alcotest.(check decode) "stored"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Alcotest.(check string) "0x00"
    "\x00" (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let length_extra () =
  Alcotest.test_case "length extra" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\xed\xc0\x01\x01\x00\x00\x00\x40\x20\xff\x57\x1b\x42\x2c\x4f") ~o ~w in
  Alcotest.(check decode) "length extra"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Alcotest.(check string) "0x00 * 516"
    (String.make 516 '\x00') (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let long_distance_and_extra () =
  Alcotest.test_case "long distance and extra" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\xed\xcf\xc1\xb1\x2c\x47\x10\xc4\x30\xfa\x6f\x35\x1d\x01\x82\x59\x3d\xfb\xbe\x2e\x2a\xfc\x0f\x0c") ~o ~w in
  Alcotest.(check decode) "long distance and extra"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Alcotest.(check string) "0x00 * 518"
    (String.make 518 '\x00') (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let window_end () =
  Alcotest.test_case "window end" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\xed\xc0\x81\x00\x00\x00\x00\x80\xa0\xfd\xa9\x17\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06") ~o ~w in
  Alcotest.(check decode) "window end"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Fmt.epr "%S.\n%!" (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder)) ;
  Alcotest.(check string) "0x00 * 33025"
    (String.make 33025 '\x00') (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let () =
  Alcotest.run "z"
    [ "invalids", [ invalid_complement_of_length ()
                  ; invalid_kind_of_block ()
                  ; invalid_code_lengths ()
                  ; invalid_bit_length_repeat ()
                  ; invalid_codes ()
                  ; invalid_lengths ()
                  ; invalid_distances ()
                  ; too_many_length_or_distance_symbols ()
                  ; invalid_distance_code ()
                  ; invalid_distance_too_far_back () ]
    ; "valids", [ fixed ()
                ; stored ()
                ; length_extra ()
                ; long_distance_and_extra ()
                ; window_end () ] ]
