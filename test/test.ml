let w = Z.bigstring_create Z.Window.max
let o = Z.bigstring_create Z.io_buffer_size

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

let str = Alcotest.testable pp_string String.equal

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

let encode ~kind lst =
  let res = Buffer.create 16 in
  let encoder = Z.N.encoder (`Buffer res) kind in
  List.iter (fun v -> match Z.N.encode encoder v with
      | `Ok -> ()
      | `Partial -> Alcotest.fail "Impossible `Partial case")
    lst ;
  Buffer.contents res

let encode_dynamic lst =
  let literals = Z.N.make_literals () in
  let distances = Z.N.make_distances () in
  List.iter
    (function
     | `Literal chr -> Z.N.succ_literal literals chr
     | `Copy (off, len) ->
        Z.N.succ_length literals len ;
        Z.N.succ_distance distances off
     | _ -> ())
    lst ;
  let dynamic = Z.N.dynamic_of_frequencies ~literals ~distances in
  encode ~kind:(Z.N.Dynamic dynamic) lst

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
    (Z.M.decode decoder) `Flush ;
  Alcotest.(check decode) "invalid distance too far back"
    (Z.M.decode decoder) (`Malformed "Invalid distance")

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

let unroll_inflate decoder =
  let buf = Buffer.create 16 in
  let rec go () = match Z.M.decode decoder with
    | `Flush ->
       for i = 0 to Z.io_buffer_size - Z.M.dst_rem decoder - 1
       do Buffer.add_char buf (Char.unsafe_chr (Z.unsafe_get_uint8 o i)) done ;
       Z.M.flush decoder ; go ()
    | `Await -> Alcotest.fail "Impossible `Await case"
    | `Malformed err -> Alcotest.fail err
    | `End ->
      for i = 0 to Z.io_buffer_size - Z.M.dst_rem decoder - 1
      do Buffer.add_char buf (Char.unsafe_chr (Z.unsafe_get_uint8 o i)) done ;
      Buffer.contents buf in
  go ()

let length_extra () =
  Alcotest.test_case "length extra" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\xed\xc0\x01\x01\x00\x00\x00\x40\x20\xff\x57\x1b\x42\x2c\x4f") ~o ~w in
  let res = unroll_inflate decoder in
  Fmt.epr "Buffer length: %d.\n%!" (String.length res) ;
  Alcotest.(check string) "0x00 * 516"
    (String.make 516 '\x00') res

let long_distance_and_extra () =
  Alcotest.test_case "long distance and extra" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\xed\xcf\xc1\xb1\x2c\x47\x10\xc4\x30\xfa\x6f\x35\x1d\x01\x82\x59\x3d\xfb\
                                      \xbe\x2e\x2a\xfc\x0f\x0c") ~o ~w in
  let res = unroll_inflate decoder in
  Alcotest.(check string) "0x00 * 518"
    (String.make 518 '\x00') res

let window_end () =
  Alcotest.test_case "window end" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\xed\xc0\x81\x00\x00\x00\x00\x80\xa0\xfd\xa9\x17\xa9\x00\x00\x00\x00\x00\
                                      \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                                      \x00\x00\x00\x00\x00\x00\x00\x00\x00\x06") ~o ~w in
  let res = unroll_inflate decoder in
  Alcotest.(check string) "0x00 * 33025"
    (String.make 33025 '\x00') res

let fuzz0 () =
  Alcotest.test_case "fuzz0" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "{\220\n s\017\027\211\\\006\211w\176`\142\2007\156oZBo\163\136\017\247\
                                      \158\247\012e\241\234sn_$\210\223\017\213\138\147]\129M\137<\242\1867\021\
                                      c\194\156\135\194\167-wo\006\200\198") ~o ~w in
  Alcotest.(check decode) "fuzz0"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Alcotest.(check string) "fuzz0"
    "\xe3\x85" (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let fuzz1 () =
  Alcotest.test_case "fuzz1" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\019\208nO\200\189r\020\176") ~o ~w in
  Alcotest.(check decode) "fuzz1"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Alcotest.(check string) "fuzz1"
    "\016+\135`m\212\197" (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let fuzz2 () =
  let expected_output =
    [ "\x1a\xca\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e" (* ..~~~~~~~~~~~~~~ *)
    ; "\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e" (* ~~~~~~~~~~~~~~~~ *)
    ; "\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e" (* ~~~~~~~~~~~~~~~~ *)
    ; "\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x3a\x2c\x50"                     (* ~~~~~~~~:,P *)      ] in
  Alcotest.test_case "fuzz2" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x93\x3a\x55\x47\x12\x80\x51\x56\x3a\x01\x00\x00") ~o ~w in
  let res = unroll_inflate decoder in
  Alcotest.(check string) "fuzz2"
    (String.concat "" expected_output) res

let fuzz3 () =
  let expected_output =
    [ "\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a" (* ..~..~..~..~..~. *)
    ; "\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca" (* .~..~..~..~..~.. *)
    ; "\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e" (* ~..~..~..~..~..~ *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76" (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76"                                                 (* .v.v *)             ] in
  Alcotest.test_case "fuzz3" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x93\x3a\x55\x47\x12\x3a\x51\x36\x0a\x01\x00\x00") ~o ~w in
  let res = unroll_inflate decoder in
  Alcotest.(check string) "fuzz3"
    (String.concat "" expected_output) res

let fuzz4 () =
  let expected_output =
    [ "\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a" (* ..~..~..~..~..~. *)
    ; "\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca" (* .~..~..~..~..~.. *)
    ; "\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e" (* ~..~..~..~..~..~ *)
    ; "\xc8\x76\x75\x75\x75\x75\x75\x75"                                 (* .vuuuuuu *)         ] in
  Alcotest.test_case "fuzz4" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x93\x3a\x55\x47\x12\x3a\x51\x56\x0a\x06\x80\x00") ~o ~w in
  let res = unroll_inflate decoder in
  Alcotest.(check string) "fuzz4"
    (String.concat "" expected_output) res

let fuzz5 () =
  let input =
    [ "\x93\x3a\x55\x01\x01\x01\x01\xe6\x01\x01\x01\x01\x01\x01\x01\x01" (* .:U............. *)
    ; "\x01\x01\x01\x01\x01\x00\x00"                                     (* ....... *)          ] in
  let expected_output =
    [ "\x1a\xca\x78\x78\x78\x78\x78\x78\x78\x50\x50\x37\x50\x50\x50\x50" (* ..xxxxxxxPP7PPPP *)
    ; "\x50\x50\x50\x50\x50\x50\x50\x50\x50"                             (* PPPPPPPPP *)        ] in
  Alcotest.test_case "fuzz5" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String (String.concat "" input)) ~o ~w in
  let res = unroll_inflate decoder in
  Alcotest.(check string) "fuzz5"
    (String.concat "" expected_output) res

let fuzz6 () =
  let expected_output =
    [ "\x19\x59\x59\x59\x5e\xe3\x59\x5e\xe3\x59\x5e\xe3\x59\x5e\xe3\x59" (* .YYY^.Y^.Y^.Y^.Y *)
    ; "\x5e\xe3\x33"                                                     (* ^.3 *)              ] in
  Alcotest.test_case "fuzz6" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x93\x8c\x8c\x8c\x8c\x7b\x8c\x8c\x8c\x01\x00\x00") ~o ~w in
  Alcotest.(check decode) "fuzz6"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Alcotest.(check string) "fuzz6"
    (String.concat "" expected_output) (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let fuzz7 () =
  Alcotest.test_case "fuzz7" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x93\x3a\x55\x69\x12\x3a\x3f\x10\x08\x01\x00\x00") ~o ~w in
  Alcotest.(check decode) "fuzz7"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End ;
  Alcotest.(check string) "fuzz7"
    "\x1a\xca\x79\x34\x55\x9f\x51\x9f\x51\x9f"
    (Bigstringaf.substring o ~off:0 ~len:(Bigstringaf.length o - Z.M.dst_rem decoder))

let fuzz8 () =
  Alcotest.test_case "fuzz8" `Quick @@ fun () ->
  let decoder = Z.M.decoder (`String "\x7a\x37\x6d\x99\x13") ~o ~w in
  Alcotest.(check decode) "fuzz8"
    (Z.M.decode decoder) (`Malformed "Unexpected end of input")

let fuzz9 () =
  Alcotest.test_case "fuzz9" `Quick @@ fun () ->
  let inputs =
    [ "\x9b\x01\x95\xfc\x51\xd2\xed\xc8\xce\xc8\xff\x80\x00\x00\x7f\xff" (* ....Q........... *)
    ; "\x79\x2f\xe9\x51\x88\x7b\xb8\x2f\xef\xa5\x8c\xf8\xf1\xb6\xce\xc8" (* y/.Q.{./........ *)
    ; "\xb8\xc8\xff\x2f\x00\x7f\x88\x7b\xbc"                             (* .../...{. *)        ] in
  let decoder = Z.M.decoder (`String (String.concat "" inputs)) ~o ~w in
  Alcotest.(check decode) "fuzz9"
    (Z.M.decode decoder) (`Malformed "Invalid distance")

let huffman_length_extra () =
  Alcotest.test_case "huffman length extra" `Quick @@ fun () ->
  let literals = Z.N.make_literals () in
  Z.N.succ_literal literals '\000' ;
  Z.N.succ_literal literals '\000' ;
  Z.N.succ_length literals 258 ;
  Z.N.succ_length literals 256 ;
  let distances = Z.N.make_distances () in
  Z.N.succ_distance distances 1 ;
  Z.N.succ_distance distances 1 ;
  let dynamic = Z.N.dynamic_of_frequencies ~literals ~distances in
  let res = encode ~kind:(Z.N.Dynamic dynamic) [ `Literal '\000'
                                               ; `Literal '\000'
                                               ; `Copy (1, 258)
                                               ; `Copy (1, 256)
                                               ; `End ] in
  Alcotest.(check str) "encoding" res "\237\193\001\001\000\000\000@ \255W\027B\193\234\004" ;

  let decoder = Z.M.decoder (`String res) ~o ~w in
  let res = unroll_inflate decoder in
  Alcotest.(check str) "result"
    (String.make (258 + 256 + 2) '\000') res

let fuzz10 () =
  Alcotest.test_case "fuzz10" `Quick @@ fun () ->
  let lst =
    [ `Literal (Char.chr 231); `Literal (Char.chr 60); `Literal (Char.chr 128)
    ; `Copy (1, 19); `End ] in
  let res = encode_dynamic lst in
  let decoder = Z.M.decoder (`String res) ~o ~w in
  Alcotest.(check decode) "decoding"
    (ignore @@ Z.M.decode decoder ; Z.M.decode decoder) `End

let fuzz11 () =
  Alcotest.test_case "fuzz11" `Quick @@ fun () ->
  let lst =
    [ `Literal (Char.chr 228)
    ; `Literal (Char.chr 255)
    ; `Copy (1, 130)
    ; `End ] in
  let res = encode_dynamic lst in
  let decoder = Z.M.decoder (`String res) ~o ~w in
  let res0 = unroll_inflate decoder in
  let res1 = Bytes.create 132 in
  Bytes.set res1 0 (Char.chr 228) ;
  Bytes.fill res1 1 131 (Char.chr 255) ;
  Alcotest.(check str) "result" res0 (Bytes.unsafe_to_string res1)

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
                ; window_end ()
                ; huffman_length_extra () ]
    ; "fuzz", [ fuzz0 ()
              ; fuzz1 ()
              ; fuzz2 ()
              ; fuzz3 ()
              ; fuzz4 ()
              ; fuzz5 ()
              ; fuzz6 ()
              ; fuzz7 ()
              ; fuzz8 ()
              ; fuzz9 ()
              ; fuzz10 ()
              ; fuzz11 () ] ]
