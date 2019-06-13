let () = Printexc.record_backtrace true

(* XXX(dinosaure): prelude. *)

[@@@warning "-32-34-37"]

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l
let bigstring_length x = Bigarray.Array1.dim x [@@inline]
let bigstring_sub x off len = Bigarray.Array1.sub x off len [@@inline]
let bigstring_blit a b = Bigarray.Array1.blit a b [@@inline]
let bigstring_to_string = Bigstringaf.to_string
let bigstring_of_string x = Bigstringaf.of_string ~off:0 ~len:(String.length x) x

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_char : bigstring -> int -> char = "%caml_ba_ref_1"
external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external unsafe_set_uint16 : bigstring -> int -> int -> unit = "%caml_bigstring_set16"
external swap : int -> int = "%bswap16"

(* XXX(dinosaure): little-endian only *)
let unsafe_set_uint16 =
  if not Sys.big_endian
  then fun buf off v -> unsafe_set_uint16 buf off v
  else fun buf off v -> unsafe_set_uint16 buf off (swap v)

external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_set_uint32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"

let output_bigstring oc buf off len =
  let tmp = Bigstringaf.substring buf ~off ~len in
  output_string oc tmp

let input_bigstring ic buf off len =
  let tmp = Bytes.create len in
  let res = input ic tmp 0 len in
  Bigstringaf.blit_from_bytes tmp ~src_off:0 buf ~dst_off:off ~len:res ; res

let invalid_bounds off len = Fmt.invalid_arg "Out of bounds (off: %d, len: %d)" off len

let unsafe_blit src src_off dst dst_off len =
  for i = 0 to len - 1
  do unsafe_set_uint8 dst (dst_off + i) (unsafe_get_uint8 src (src_off + i)) done

let slow_blit2 src src_off dst0 dst0_off dst1 dst1_off len =
  for i = 0 to len - 1
  do
    let v = unsafe_get_uint8 src (src_off + i) in
    unsafe_set_uint8 dst0 (dst0_off + i) v ;
    unsafe_set_uint8 dst1 (dst1_off + i) v ;
  done

(* XXX(dinosaure): fast blit when it's possible. *)

let blit2 src src_off dst0 dst0_off dst1 dst1_off len =
  if dst0_off - src_off < 4
  then slow_blit2 src src_off dst0 dst0_off dst1 dst1_off len
  else
    let len0 = len land 3 in
    let len1 = len asr 2 in

    for i = 0 to len1 - 1
    do
      let i = i * 4 in
      let v = unsafe_get_uint32 src (src_off + i) in
      unsafe_set_uint32 dst0 (dst0_off + i) v ;
      unsafe_set_uint32 dst1 (dst1_off + i) v ;
    done ;

    for i = 0 to len0 - 1
    do
      let i = len1 * 4 + i in
      let v = unsafe_get_uint8 src (src_off + i) in
      unsafe_set_uint8 dst0 (dst0_off + i) v ;
      unsafe_set_uint8 dst1 (dst1_off + i) v ;
    done

(* XXX(dinosaure): fast fill operation. (usually when [Match (len:?, dist:1)]) *)

let fill2 v dst0 dst0_off dst1 dst1_off len =
  let len0 = len land 3 in
  let len1 = len asr 2 in

  let nv = Nativeint.of_int v in
  let vv = Nativeint.(logor (shift_left nv 8) nv) in
  let vvvv = Nativeint.(logor (shift_left vv 16) vv) in
  let vvvv = Nativeint.to_int32 vvvv in

  for i = 0 to len1 - 1
  do
    let i = i * 4 in
    unsafe_set_uint32 dst0 (dst0_off + i) vvvv ;
    unsafe_set_uint32 dst1 (dst1_off + i) vvvv
  done ;

  for i = 0 to len0 - 1
  do
    let i = len1 * 4 + i in
    unsafe_set_uint8 dst0 (dst0_off + i) v ;
    unsafe_set_uint8 dst1 (dst1_off + i) v
  done

let io_buffer_size = 65536

(* XXX(dinosaure): Specialization. *)

external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let ( > ) (x : int) y = x > y [@@inline]
let ( < ) (x : int) y = x < y [@@inline]
let ( <= ) (x : int) y = x <= y [@@inline]
let ( >= ) (x : int) y = x >= y [@@inline]

let min (a : int) b = if a <= b then a else b [@@inline]

(* XXX(dinosaure): Constants. *)

let _max_bits = 15
let _smallest = 1
let _rep_3_6 = 16
let _repz_3_10 = 17
let _repz_11_138 = 18
let _literals = 256
let _length_codes = 29
let _l_codes = _literals + 1 + _length_codes
let _d_codes = 30
let _heap_size = 2 * _l_codes + 1
let _bl_codes = 19

let zigzag = [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]

let _length =
  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12; 13
   ; 13; 13; 13; 14; 14; 14; 14; 15; 15; 15; 15; 16; 16; 16; 16; 16; 16; 16
   ; 16; 17; 17; 17; 17; 17; 17; 17; 17; 18; 18; 18; 18; 18; 18; 18; 18; 19
   ; 19; 19; 19; 19; 19; 19; 19; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20
   ; 20; 20; 20; 20; 20; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21
   ; 21; 21; 21; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22
   ; 22; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 24
   ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24
   ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 25; 25; 25; 25; 25
   ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25
   ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; 28 |]

let _distance code =
  let t =
    [| 0; 1; 2; 3; 4; 4; 5; 5; 6; 6; 6; 6; 7; 7; 7; 7; 8; 8; 8; 8; 8; 8; 8; 8
     ; 9; 9; 9; 9; 9; 9; 9; 9; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10
     ; 10; 10; 10; 10; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11
     ; 11; 11; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12
     ; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 13; 13
     ; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13
     ; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 14; 14; 14; 14; 14; 14
     ; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14
     ; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14
     ; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14
     ; 14; 14; 14; 14; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15
     ; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15
     ; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15
     ; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15;  0;  0; 16; 17
     ; 18; 18; 19; 19; 20; 20; 20; 20; 21; 21; 21; 21; 22; 22; 22; 22; 22; 22
     ; 22; 22; 23; 23; 23; 23; 23; 23; 23; 23; 24; 24; 24; 24; 24; 24; 24; 24
     ; 24; 24; 24; 24; 24; 24; 24; 24; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25
     ; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
     ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
     ; 26; 26; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
     ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 28; 28
     ; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28
     ; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28
     ; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28
     ; 28; 28; 28; 28; 28; 28; 28; 28; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29
     ; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29
     ; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29
     ; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29 |]
  in
  if code < 256 then t.(code) else t.(256 + (code lsr 7)) [@@inline]

let _base_length =
  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 10; 12; 14; 16; 20; 24; 28; 32; 40; 48; 56; 64
   ; 80; 96; 112; 128; 160; 192; 224; 255 |]

let _extra_lbits =
  [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5
   ; 5; 5; 5; 0 |]

let _extra_dbits =
  [| 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10
   ; 11; 11; 12; 12; 13; 13; 0; 0 |]

let _base_dist =
  [| 0; 1; 2; 3; 4; 6; 8; 12; 16; 24; 32; 48; 64; 96; 128; 192; 256; 384; 512
   ; 768; 1024; 1536; 2048; 3072; 4096; 6144; 8192; 12288; 16384; 24576; (-1); (-1) |]

module Lookup = struct
  type t =
    { t : int array
    ; m : int
    ; l : int }

  let mask = (1 lsl _max_bits) - 1
  let make t m = { t; m= (1 lsl m) - 1; l= m }

  let get t i =
    let v = t.t.(i) in v lsr _max_bits, v land mask (* allocation *)
  [@@inline]

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>t = @[<hov>%a@];@ m = %x;@ l = %d;@] }"
      Fmt.(Dump.array int) t.t t.m t.l
end

let _static_ltree =
  [|  (12, 8); (140, 8);  (76, 8); (204, 8);  (44, 8); (172, 8); (108, 8)
   ; (236, 8);  (28, 8); (156, 8);  (92, 8); (220, 8);  (60, 8); (188, 8)
   ; (124, 8); (252, 8);   (2, 8); (130, 8);  (66, 8); (194, 8);  (34, 8)
   ; (162, 8);  (98, 8); (226, 8);  (18, 8); (146, 8);  (82, 8); (210, 8)
   ;  (50, 8); (178, 8); (114, 8); (242, 8);  (10, 8); (138, 8);  (74, 8)
   ; (202, 8);  (42, 8); (170, 8); (106, 8); (234, 8);  (26, 8); (154, 8)
   ;  (90, 8); (218, 8);  (58, 8); (186, 8); (122, 8); (250, 8);   (6, 8)
   ; (134, 8);  (70, 8); (198, 8);  (38, 8); (166, 8); (102, 8); (230, 8)
   ;  (22, 8); (150, 8);  (86, 8); (214, 8);  (54, 8); (182, 8); (118, 8)
   ; (246, 8);  (14, 8); (142, 8);  (78, 8); (206, 8);  (46, 8); (174, 8)
   ; (110, 8); (238, 8);  (30, 8); (158, 8);  (94, 8); (222, 8);  (62, 8)
   ; (190, 8); (126, 8); (254, 8);   (1, 8); (129, 8);  (65, 8); (193, 8)
   ;  (33, 8); (161, 8);  (97, 8); (225, 8);  (17, 8); (145, 8);  (81, 8)
   ; (209, 8);  (49, 8); (177, 8); (113, 8); (241, 8);   (9, 8); (137, 8)
   ;  (73, 8); (201, 8);  (41, 8); (169, 8); (105, 8); (233, 8);  (25, 8)
   ; (153, 8);  (89, 8); (217, 8);  (57, 8); (185, 8); (121, 8); (249, 8)
   ;   (5, 8); (133, 8);  (69, 8); (197, 8);  (37, 8); (165, 8); (101, 8)
   ; (229, 8);  (21, 8); (149, 8);  (85, 8); (213, 8);  (53, 8); (181, 8)
   ; (117, 8); (245, 8);  (13, 8); (141, 8);  (77, 8); (205, 8);  (45, 8)
   ; (173, 8); (109, 8); (237, 8);  (29, 8); (157, 8);  (93, 8); (221, 8)
   ;  (61, 8); (189, 8); (125, 8); (253, 8);  (19, 9); (275, 9); (147, 9)
   ; (403, 9);  (83, 9); (339, 9); (211, 9); (467, 9);  (51, 9); (307, 9)
   ; (179, 9); (435, 9); (115, 9); (371, 9); (243, 9); (499, 9);  (11, 9)
   ; (267, 9); (139, 9); (395, 9);  (75, 9); (331, 9); (203, 9); (459, 9)
   ;  (43, 9); (299, 9); (171, 9); (427, 9); (107, 9); (363, 9); (235, 9)
   ; (491, 9);  (27, 9); (283, 9); (155, 9); (411, 9);  (91, 9); (347, 9)
   ; (219, 9); (475, 9);  (59, 9); (315, 9); (187, 9); (443, 9); (123, 9)
   ; (379, 9); (251, 9); (507, 9);   (7, 9); (263, 9); (135, 9); (391, 9)
   ;  (71, 9); (327, 9); (199, 9); (455, 9);  (39, 9); (295, 9); (167, 9)
   ; (423, 9); (103, 9); (359, 9); (231, 9); (487, 9);  (23, 9); (279, 9)
   ; (151, 9); (407, 9);  (87, 9); (343, 9); (215, 9); (471, 9);  (55, 9)
   ; (311, 9); (183, 9); (439, 9); (119, 9); (375, 9); (247, 9); (503, 9)
   ;  (15, 9); (271, 9); (143, 9); (399, 9);  (79, 9); (335, 9); (207, 9)
   ; (463, 9);  (47, 9); (303, 9); (175, 9); (431, 9); (111, 9); (367, 9)
   ; (239, 9); (495, 9);  (31, 9); (287, 9); (159, 9); (415, 9);  (95, 9)
   ; (351, 9); (223, 9); (479, 9);  (63, 9); (319, 9); (191, 9); (447, 9)
   ; (127, 9); (383, 9); (255, 9); (511, 9);   (0, 7);  (64, 7);  (32, 7);  (96, 7)
   ;  (16, 7);  (80, 7);  (48, 7); (112, 7);   (8, 7);  (72, 7);  (40, 7); (104, 7)
   ;  (24, 7);  (88, 7);  (56, 7); (120, 7);   (4, 7);  (68, 7);  (36, 7); (100, 7)
   ;  (20, 7);  (84, 7);  (52, 7); (116, 7);   (3, 8); (131, 8);  (67, 8); (195, 8)
   ;  (35, 8); (163, 8);  (99, 8); (227, 8) |]

let _static_ltree =
  let t = Array.map (fun (v, l) -> (l lsl _max_bits) lor v) _static_ltree in
  Lookup.make t 9

let _static_dtree =
  [| (0, 5); (16, 5);  (8, 5); (24, 5); (4, 5); (20, 5); (12, 5); (28, 5)
   ; (2, 5); (18, 5); (10, 5); (26, 5); (6, 5); (22, 5); (14, 5); (30, 5)
   ; (1, 5); (17, 5);  (9, 5); (25, 5); (5, 5); (21, 5); (13, 5); (29, 5)
   ; (3, 5); (19, 5); (11, 5); (27, 5); (7, 5); (23, 5) |]

let _static_dtree =
  let t = Array.map (fun (v, l) -> (l lsl _max_bits) lor v) _static_dtree in
  Lookup.make t 5

(* XXX(dinosaure): [zlib] raises "Invalid distance code" where it wants to
   access to [base_dist.(30|31)]. It uses a smart mask to catch this behavior.
   In this code, we did not raise an error nor /compromise/ output when we
   fall to [Match (len:?, dist:0)] (so, nothing to do).

   Case can be retrieved with "\x02\x7e\xff\xff". NOTE: [miniz] has this
   silent behavior. *)

type optint = Optint.t

(* XXX(dinosaure): optimize [Heap]. *)

module Heap = struct
  type priority = int
  type 'a queue = None | Node of priority * 'a * 'a queue * 'a queue

  let rec push queue priority elt =
    match queue with
    | None -> Node (priority, elt, None, None)
    | Node (p, e, left, right) ->
        if priority <= p then Node (priority, elt, push right p e, left)
        else Node (p, e, push right priority elt, left)

  exception Empty

  let rec remove = function
    | None -> raise_notrace Empty
    | Node (_, _, left, None) -> left
    | Node (_, _, None, right) -> right
    | Node
        (_, _, (Node (lp, le, _, _) as left), (Node (rp, re, _, _) as right))
      ->
        if lp <= rp then Node (lp, le, remove left, right)
        else Node (rp, re, left, remove right)

  let take = function
    | None -> raise_notrace Empty
    | Node (p, e, _, _) as queue -> (p, e, remove queue)

  let rec pp pp_data ppf = function
    | None -> Fmt.string ppf "<none>"
    | Node (p, e, l, r) ->
      Fmt.pf ppf "(Node (priority: %d, e: %a, l: %a, r: %a))"
        p pp_data e (Fmt.hvbox (pp pp_data)) l (Fmt.hvbox (pp pp_data)) r
end

module Window = struct
  type t =
    { raw : bigstring
    ; mutable w : int
    ; mutable c : Optint.t }

  let max = 1 lsl 15
  let mask = (1 lsl 15) - 1
  let ( = ) (a : int) b = a == b

  let make () =
    { raw= Bigarray.Array1.create Bigarray.char Bigarray.c_layout max
    ; w= 0
    ; c= Checkseum.Adler32.default }

  let from raw =
    { raw; w= 0; c= Checkseum.Adler32.default }

  let mask v = v land mask
  [@@inline]

  let update w =
    let c = Checkseum.Adler32.unsafe_digest_bigstring w.raw 0 max w.c in
    w.c <- c

  let add t v =
    unsafe_set_uint8 t.raw (mask t.w) v ;
    if mask (t.w + 1) == 0 then update t ;
    t.w <- t.w + 1

  let sub a b = ( - ) a b

  let compare a b =
    (compare : int -> int -> int) (sub a min_int) (sub b min_int)

  let have t =
    if compare t.w max < 0 then t.w else max
  (* XXX(dinosaure): this code works only for an deflated input less than
     [max_int] bytes. *)

  let blit t w w_off o o_off len =
    let msk = mask t.w in
    let pre = max - msk in
    let rst = len - pre in
    if rst >= 0
    then ( blit2 w w_off t.raw msk o o_off pre
         ; update t
         ; blit2 w (w_off + pre) t.raw 0 o (o_off + pre) rst )
    else blit2 w w_off t.raw msk o o_off len ;
    t.w <- t.w + len

  let fill t v o o_off len =
    let msk = mask t.w in
    let pre = max - msk in
    let rst = len - pre in
    if rst >= 0
    then ( fill2 v t.raw msk o o_off pre
         ; update t
         ; fill2 v t.raw 0 o (o_off + pre) rst )
    else fill2 v t.raw msk o o_off len ;
    t.w <- t.w + len

  let tail w =
    let msk = mask w.w in
    if msk > 0
    then ( let c = Checkseum.Adler32.unsafe_digest_bigstring w.raw 0 msk w.c in
           w.c <- c )

  let feed t buf off len =
    let msk = mask t.w in
    let pre = max - msk in
    let rst = len - pre in
    ( if rst > 0
      then ( unsafe_blit buf off t.raw msk pre
           ; update t
           ; unsafe_blit buf (off + pre) t.raw 0 rst )
      else unsafe_blit buf off t.raw msk len ) ;
    t.w <- t.w + len

  let checksum w = w.c
end

module M = struct
  (* à la dbuenzli *)

  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Await | `Flush | `End | `Malformed of string ]

  exception Invalid_huffman

  let prefix heap max =
    assert (max < 16) ; (* allocation *)
    let tbl = Array.make (1 lsl max) 0 in
    let rec backward huff incr =
      if huff land incr <> 0 then backward huff (incr lsr 1) else incr
    in
    let rec aux huff heap =
      match Heap.take heap with
      | _, (len, value), heap ->
          let rec loop decr fill =
            tbl.(huff + fill) <- (len lsl 15) lor value ;
            if fill <> 0 then loop decr (fill - decr)
          in
          let decr = 1 lsl len in
          loop decr ((1 lsl max) - decr) ;
          let incr = backward huff (1 lsl (len - 1)) in
          aux (if incr != 0 then (huff land (incr - 1)) + incr else 0) heap
      | exception Heap.Empty -> ()
    in
    aux 0 heap ; tbl

  type kind = CODES | LENS | DISTS

  let empty_table =
    [| 1 lsl _max_bits (* len: 1, val: 0 *) |], 1

  let huffman kind table off codes =
    let bl_count = Array.make 16 0 in
    let max = ref 15 in

    for sym = 0 to codes - 1 do
      let p = table.(off + sym) in
      bl_count.(p) <- bl_count.(p) + 1
    done ;

    (* XXX(dinosaure): check if we have an incomplete set for [LENS] and [DIST].
       This code is ugly, TODO! *)
    let exception Break in

    ( try while !max >= 1 do
          if bl_count.(!max) != 0 then raise_notrace Break
        ; decr max done with Break -> () ) ;

    if !max == 0 then empty_table
    else
      ( let code = ref 0 in
        let left = ref 1 in
        let next_code = Array.make 16 0 in
        for i = 1 to 15 do
          left := !left lsl 1 ;
          left := !left - bl_count.(i) ;
          if !left < 0 then raise Invalid_huffman ;
          code := (!code + bl_count.(i)) lsl 1 ;
          next_code.(i) <- !code
        done ;
        if !left > 0 && (kind = CODES || !max != 1) then raise Invalid_huffman ;
        let ordered = ref Heap.None in
        let max = ref 0 in
        for i = 0 to codes - 1 do
          let l = table.(off + i) in
          if l <> 0 then (
            let n = next_code.(l - 1) in
            next_code.(l - 1) <- n + 1 ;
            ordered := Heap.push !ordered n (l, i) ; (* allocation *)
            max := if l > !max then l else !max )
        done ; (prefix !ordered !max, !max) (* allocation *) )

  type decoder =
    { src : src
    ; mutable i : bigstring
    ; mutable i_pos : int
    ; mutable i_len : int
    ; mutable hold : int
    ; mutable bits : int
    ; mutable last : bool
    ; o : bigstring
    ; mutable o_pos : int
    ; mutable l : int (* literal / length *)
    ; mutable d : int (* distance *)
    ; mutable literal : Lookup.t
    ; mutable distance : Lookup.t
    ; mutable jump : jump
    ; w : Window.t
    ; mutable s : state
    ; mutable k : decoder -> ret }
  and state =
    | Header
    | Table of { hlit : int
               ; hdist : int
               ; hclen : int }
    | Inflate_table of { t : int array
                       ; l : int
                       ; r : int array
                       ; h : int * int * int }
    | Inflate
    | Slow
    | Flat_header
    | Flat
    | Checkseum
    | End_of_inflate
  and jump = Length | Extra_length | Distance | Extra_distance | Write
  and ret = Await | Flush | End | K | Malformed of string

  let pp_jump ppf = function
    | Length -> Fmt.string ppf "length"
    | Extra_length -> Fmt.string ppf "extra-length"
    | Distance -> Fmt.string ppf "distance"
    | Extra_distance -> Fmt.string ppf "extra-distance"
    | Write -> Fmt.string ppf "write"

  let malformedf fmt = Fmt.kstrf (fun s -> Malformed s) fmt

  (* End of input [eoi] is signalled by [d.i_pos = 0] and [d.i_len = min_int]
     which implies [i_rem d < 0] is [true]. *)

  let eoi d =
    d.i <- bigstring_empty ;
    d.i_pos <- 0 ;
    d.i_len <- min_int

  let final _ = End

  let err_unexpected_end_of_input d =
    eoi d ; d.k <- final ;
    malformedf "Unexpected end of input"

  let err_invalid_kind_of_block d =
    eoi d ; d.k <- final ;
    malformedf "Invalid kind of block"

  let err_invalid_dictionary d =
    eoi d ; d.k <- final ;
    malformedf "Invalid dictionary"

  let err_invalid_checksum d =
    eoi d ; d.k <- final ;
    malformedf "Invalid checksum"

  let err_invalid_complement_of_length d =
    eoi d ; d.k <- final ;
    malformedf "Invalid complement of length"

  let err_invalid_distance d =
    eoi d ; d.k <- final ;
    malformedf "Invalid distance"

  (* remaining bytes to read [d.i]. *)
  let i_rem d = d.i_len - d.i_pos + 1
  [@@inline]

  (* set [d.i] with [s]. *)
  let src d s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    if (l == 0) then eoi d
    else
      ( d.i <- s
      ; d.i_pos <- j
      ; d.i_len <- j + l - 1 )

  (* get new input in [d.i] and [k]ontinue. *)
  let refill k d = match d.src with
    | `String _ ->
      eoi d ; k d
    | `Channel ic ->
      let res = input_bigstring ic d.i 0 (Bigstringaf.length d.i) in
      src d d.i 0 res ; k d
    | `Manual ->
      d.k <- k ; Await

  (* ensure to call [k] with, at least, [n] bits available. *)
  let rec c_peek_bits n k d =
    if d.bits >= n then k d
    else
      let rem = i_rem d in

      if rem <= 0
      then
        if rem < 0 (* end of input *)
        then err_unexpected_end_of_input d
        else refill (c_peek_bits n k) d (* allocation *)
      else
        ( let byte = unsafe_get_uint8 d.i d.i_pos in
          d.i_pos <- d.i_pos + 1
        ; d.hold <- d.hold lor (byte lsl d.bits)
        ; d.bits <- d.bits + 8
        ; if d.bits >= n then k d else c_peek_bits n k d )

  let reverse_bits bits =
    let t =
      [| 0x00; 0x80; 0x40; 0xC0; 0x20; 0xA0; 0x60; 0xE0
       ; 0x10; 0x90; 0x50; 0xD0; 0x30; 0xB0; 0x70; 0xF0
       ; 0x08; 0x88; 0x48; 0xC8; 0x28; 0xA8; 0x68; 0xE8
       ; 0x18; 0x98; 0x58; 0xD8; 0x38; 0xB8; 0x78; 0xF8
       ; 0x04; 0x84; 0x44; 0xC4; 0x24; 0xA4; 0x64; 0xE4
       ; 0x14; 0x94; 0x54; 0xD4; 0x34; 0xB4; 0x74; 0xF4
       ; 0x0C; 0x8C; 0x4C; 0xCC; 0x2C; 0xAC; 0x6C; 0xEC
       ; 0x1C; 0x9C; 0x5C; 0xDC; 0x3C; 0xBC; 0x7C; 0xFC
       ; 0x02; 0x82; 0x42; 0xC2; 0x22; 0xA2; 0x62; 0xE2
       ; 0x12; 0x92; 0x52; 0xD2; 0x32; 0xB2; 0x72; 0xF2
       ; 0x0A; 0x8A; 0x4A; 0xCA; 0x2A; 0xAA; 0x6A; 0xEA
       ; 0x1A; 0x9A; 0x5A; 0xDA; 0x3A; 0xBA; 0x7A; 0xFA
       ; 0x06; 0x86; 0x46; 0xC6; 0x26; 0xA6; 0x66; 0xE6
       ; 0x16; 0x96; 0x56; 0xD6; 0x36; 0xB6; 0x76; 0xF6
       ; 0x0E; 0x8E; 0x4E; 0xCE; 0x2E; 0xAE; 0x6E; 0xEE
       ; 0x1E; 0x9E; 0x5E; 0xDE; 0x3E; 0xBE; 0x7E; 0xFE
       ; 0x01; 0x81; 0x41; 0xC1; 0x21; 0xA1; 0x61; 0xE1
       ; 0x11; 0x91; 0x51; 0xD1; 0x31; 0xB1; 0x71; 0xF1
       ; 0x09; 0x89; 0x49; 0xC9; 0x29; 0xA9; 0x69; 0xE9
       ; 0x19; 0x99; 0x59; 0xD9; 0x39; 0xB9; 0x79; 0xF9
       ; 0x05; 0x85; 0x45; 0xC5; 0x25; 0xA5; 0x65; 0xE5
       ; 0x15; 0x95; 0x55; 0xD5; 0x35; 0xB5; 0x75; 0xF5
       ; 0x0D; 0x8D; 0x4D; 0xCD; 0x2D; 0xAD; 0x6D; 0xED
       ; 0x1D; 0x9D; 0x5D; 0xDD; 0x3D; 0xBD; 0x7D; 0xFD
       ; 0x03; 0x83; 0x43; 0xC3; 0x23; 0xA3; 0x63; 0xE3
       ; 0x13; 0x93; 0x53; 0xD3; 0x33; 0xB3; 0x73; 0xF3
       ; 0x0B; 0x8B; 0x4B; 0xCB; 0x2B; 0xAB; 0x6B; 0xEB
       ; 0x1B; 0x9B; 0x5B; 0xDB; 0x3B; 0xBB; 0x7B; 0xFB
       ; 0x07; 0x87; 0x47; 0xC7; 0x27; 0xA7; 0x67; 0xE7
       ; 0x17; 0x97; 0x57; 0xD7; 0x37; 0xB7; 0x77; 0xF7
       ; 0x0F; 0x8F; 0x4F; 0xCF; 0x2F; 0xAF; 0x6F; 0xEF
       ; 0x1F; 0x9F; 0x5F; 0xDF; 0x3F; 0xBF; 0x7F; 0xFF |]
    in t.(bits)
  [@@inline]

  let fixed_lit, fixed_dist =
    let tbl_lit =
      Array.init 288 @@ fun n ->
      if n < 144 then 8
      else if n < 256 then 9
      else if n < 280 then 7
      else 8 in
    let tbl_dist =
      let res = Array.make (1 lsl 5) 0 in
      Array.iteri (fun i _ -> res.(i) <- (5 lsl 15) lor reverse_bits (i lsl 3)) res ; res in
    let tbl_lit, max_lit = huffman LENS tbl_lit 0 288 in
    Lookup.make tbl_lit max_lit, Lookup.make tbl_dist 5

  (* XXX(dinosaure): [d.hold] can stores a byte from input. In this case, if we
     want a byte, we give [d.hold & 0xff]. Otherwise, we directly give byte from
     [d.i]. *)
  let hold_or_input d =
    if d.bits >= 8
    then ( d.hold <- d.hold lsr (d.bits mod 8)
         ; d.bits <- (d.bits / 8) * 8
         ; let v = d.hold land 0xff in
           d.hold <- d.hold lsr 8
         ; d.bits <- d.bits - 8
         ; v )
    else ( let v = unsafe_get_uint8 d.i d.i_pos in
           d.i_pos <- d.i_pos + 1
         ; v )

  let rec c_bytes n k d =
    let rem = i_rem d in
    if rem >= n then k d
    else
      ( if rem < 0 (* end of input *)
        then err_unexpected_end_of_input d
        else refill (c_bytes n k) d (* allocation *) )

  let checksum d =
    let k d =
      Window.tail d.w ;

      let a0 = Optint.of_int (hold_or_input d) in
      let a1 = Optint.of_int (hold_or_input d) in
      let b0 = Optint.of_int (hold_or_input d) in
      let b1 = Optint.of_int (hold_or_input d) in

      let v = Window.checksum d.w in
      let v' = Optint.Infix.(a0 << 24 || a1 << 16 || b0 << 8 || b1) in

      if Optint.equal v v'
      then
        let k d = d.k <- final ; End in
        if d.o_pos == 0 then End
        else ( d.s <- End_of_inflate
             ; d.k <- k
             ; Flush )
      else err_invalid_checksum d in
    let required = 4 - (d.bits / 8) in
    c_bytes required k d

  let flat d =
    let len = min (min (i_rem d) d.l) (bigstring_length d.o - d.o_pos) in
    Window.blit d.w d.i d.i_pos d.o d.o_pos len ;

    d.o_pos <- d.o_pos + len ;
    d.i_pos <- d.i_pos + len ;
    d.l <- d.l - len ;

    if d.l == 0
    then ( if d.last
           then ( d.s <- End_of_inflate ; End )
           else ( d.s <- Header ; K ) )
    else match i_rem d, bigstring_length d.o - d.o_pos with
      | 0, _ ->
        ( match d.src with
          | `String _ -> eoi d ; err_unexpected_end_of_input d
          | `Channel _ -> assert false (* TODO *)
          | `Manual -> Await)
      | _, 0 -> Flush
      | _, _ -> assert false

  let flat_header d =
    let k d =
      let len0 = hold_or_input d in
      let len1 = hold_or_input d in
      let nlen0 = hold_or_input d in
      let nlen1 = hold_or_input d in

      let len = len0 lor (len1 lsl 8) in
      let nlen = nlen0 lor (nlen1 lsl 8) in

      if nlen != 0xffff - len
      then err_invalid_complement_of_length d
      else ( d.hold <- 0 ; d.bits <- 0 ; d.l <- len ; d.s <- Flat ; flat d ) in
    let required = 4 - (d.bits / 8) in
    c_bytes required k d

  let n_extra_lbits = Array.map Nativeint.of_int _extra_lbits
  let n_extra_dbits = Array.map Nativeint.of_int _extra_dbits

  let rec c_put_byte byte k d =
    if d.o_pos < bigstring_length d.o
    then ( unsafe_set_uint8 d.o d.o_pos byte
         ; Window.add d.w byte
         ; d.o_pos <- d.o_pos + 1
         ; k d )
    else ( d.k <- c_put_byte byte k (* allocation *)
         ; Flush )

  let[@inline always] is_end_of_block lit d =
    let rem = i_rem d in
    lit.Lookup.t.(d.hold land lit.Lookup.m) land Lookup.mask == 256
    && lit.Lookup.t.(d.hold land lit.Lookup.m) lsr 15 <= d.bits
    && rem < 1

  let slow_inflate lit dist jump d =
    let rec c_peek_bits n k d =
      if d.bits >= n then k d
      else
        let rem = i_rem d in

        if rem <= 0
        then
          if rem < 0 (* end of input *)
          then err_unexpected_end_of_input d
          else refill (c_peek_bits n k) d (* allocation *)
        else
          ( let byte = unsafe_get_uint8 d.i d.i_pos in
            d.i_pos <- d.i_pos + 1
          ; d.hold <- d.hold lor (byte lsl d.bits)
          ; d.bits <- d.bits + 8
          ; if d.bits >= n || is_end_of_block lit d then k d else c_peek_bits n k d ) in

    match jump with
    | Length ->
      let k d =
        let value = lit.Lookup.t.(d.hold land lit.Lookup.m) land Lookup.mask in
        let len = lit.Lookup.t.(d.hold land lit.Lookup.m) lsr 15 in
        d.hold <- d.hold lsr len ;
        d.bits <- d.bits - len ;

        if value < 256
        then
          let k d =
            d.s <- Inflate ; (* allocation *)
            K in
          c_put_byte value k d
        else if value == 256
        then ( if d.last
               then ( d.s <- End_of_inflate ; End )
               else ( d.s <- Header ; K ) )
        else ( d.l <- value - 257
             ; d.jump <- Extra_length
             ; d.s <- Inflate (* allocation *)
             ; K ) in
      (* XXX(dinosaure): this is necessary where [EOB] is not necessary the
         longest code. So we can occur the case where we are at the end of the
         input and have the [EOB] code, but not enough to have [lit.Lookup.l]
         bits:

         - previously, we just ask more input
         - now, we check if [d.hold] is [EOB]: assumption, codes are prefix free
           AND we reach end of input.

         TODO: optimize this branch! *)
      if is_end_of_block lit d
      then k d
      else c_peek_bits lit.Lookup.l k d
    | Extra_length ->
      let len = _extra_lbits.(d.l) in
      let k d =
        let extra = d.hold land ((1 lsl len) - 1) in
        d.hold <- d.hold lsr len ;
        d.bits <- d.bits - len ;
        d.l <- _base_length.(d.l) + 3 + extra ;
        d.jump <- Distance ;
        d.s <- Inflate ; (* allocation *)
        K in
      c_peek_bits len k d
    | Distance ->
      let k d =
        let value = dist.Lookup.t.(d.hold land dist.Lookup.m) land Lookup.mask in
        let len = dist.Lookup.t.(d.hold land dist.Lookup.m) lsr 15 in

        d.hold <- d.hold lsr len ;
        d.bits <- d.bits - len ;
        d.d <- value ;
        d.jump <- Extra_distance ;
        d.s <- Inflate ; (* allocation *)
        K in
      c_peek_bits dist.Lookup.l k d
    | Extra_distance ->
      let len = _extra_dbits.(d.d) in
      let k d =
        let extra = d.hold land ((1 lsl len) - 1) in
        d.hold <- d.hold lsr len ;
        d.bits <- d.bits - len ;
        d.d <- _base_dist.(d.d) + 1 + extra ;
        d.jump <- Write ;
        d.s <- Inflate ; (* allocation *)
        K in
      c_peek_bits len k d
    | Write ->
      if d.d > Window.have d.w
      then err_invalid_distance d
      else
        let len = min d.l (bigstring_length d.o - d.o_pos) in
        let off = Window.mask (d.w.Window.w - d.d) in
        let pre = Window.max - off in
        let rst = len - pre in
        if rst > 0
        then ( Window.blit d.w d.w.Window.raw off d.o d.o_pos pre
                ; Window.blit d.w d.w.Window.raw 0 d.o (d.o_pos + pre) rst )
        else Window.blit d.w d.w.Window.raw off d.o d.o_pos len ;
        d.o_pos <- d.o_pos + len ;
        if d.l - len == 0
        then ( d.jump <- Length
             ; d.s <- Inflate (* allocation *)
             ; K )
        else ( d.l <- d.l - len
             ; d.s <- Inflate (* allocation *)
             ; Flush )

  let inflate lit dist jump d =
    let exception End in
    let exception Invalid_distance in

    let hold = ref (Nativeint.of_int d.hold) in
    let bits = ref d.bits in
    let jump = ref jump in
    let i_pos = ref d.i_pos in
    let o_pos = ref d.o_pos in

    let lit_mask = Nativeint.of_int lit.Lookup.m in
    let dist_mask = Nativeint.of_int dist.Lookup.m in

    (* XXX(dinosaure): 2 jumps were done in this hot-loop:
       1- [while],
       2- [match .. with]).

       A [let rec length = .. and extra_length = ..] can be optimized by
       [flambda]. We should replace [match .. with] by this design. TODO. *)

    try while d.i_len - !i_pos + 1 > 1
              && !o_pos < bigstring_length d.o
      do match !jump with
        | Length ->
          if !bits < lit.Lookup.l
          then ( hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
               ; bits := !bits + 8
               ; incr i_pos
               ; hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
               ; bits := !bits + 8
               ; incr i_pos ) ;
          let value = lit.Lookup.t.(Nativeint.(to_int (logand !hold lit_mask))) land Lookup.mask in
          let len = lit.Lookup.t.(Nativeint.(to_int (logand !hold lit_mask))) lsr 15 in
          hold := Nativeint.shift_right_logical !hold len ;
          bits := !bits - len ;

          if value < 256
          then ( unsafe_set_uint8 d.o !o_pos value
               ; Window.add d.w value
               ; incr o_pos
               (* ; jump := Length *) )
          else if value == 256 then raise_notrace End
          else ( jump := Extra_length
               ; d.l <- value - 257 )
        | Extra_length ->
          let len = _extra_lbits.(d.l) in
          if !bits < len
          then ( hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
               ; bits := !bits + 8
               ; incr i_pos ) ;
          let extra = Nativeint.(to_int (logand !hold (sub (shift_left 1n len) 1n))) in

          hold := Nativeint.shift_right_logical !hold len ;
          bits := !bits - len ;
          d.l <- _base_length.(d.l) + 3 + extra ;
          jump := Distance
        | Distance ->
          if !bits < dist.Lookup.l
          then ( hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
               ; bits := !bits + 8
               ; incr i_pos
               ; hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
               ; bits := !bits + 8
              ; incr i_pos ) ;
          let value = dist.Lookup.t.(Nativeint.(to_int (logand !hold dist_mask))) land Lookup.mask in
          let len = dist.Lookup.t.(Nativeint.(to_int (logand !hold dist_mask))) lsr 15 in

          hold := Nativeint.shift_right_logical !hold len ;
          bits := !bits - len ;
          d.d <- value ;
          jump := Extra_distance
        | Extra_distance ->
          let len = _extra_dbits.(d.d) in
          if !bits < len
          then ( hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
               ; bits := !bits + 8
               ; incr i_pos
               ; hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
               ; bits := !bits + 8
               ; incr i_pos ) ;
          let extra = Nativeint.(to_int (logand !hold (sub (shift_left 1n len) 1n))) in
          hold := Nativeint.shift_right_logical !hold len ;
          bits := !bits - len ;
          d.d <- _base_dist.(d.d) + 1 + extra ;

          (* if d.d > Window.have d.w then raise Invalid_distance ;
             XXX(dinosaure): [Window.have] does not tell me the truth where
             we need a read cursor in [Window.t] for that. *)

          jump := Write
        | Write ->
          if d.d > Window.have d.w then raise_notrace Invalid_distance ;

          let len = min d.l (bigstring_length d.o - !o_pos) in
          let off = Window.mask (d.w.Window.w - d.d) in

          if d.d == 1
          then
            ( let v = unsafe_get_uint8 d.w.Window.raw off in
              Window.fill d.w v d.o !o_pos len )
          else
            ( let off = Window.mask (d.w.Window.w - d.d) in
              let pre = Window.max - off in
              let rst = len - pre in
              if rst > 0
              then ( Window.blit d.w d.w.Window.raw off d.o !o_pos pre
                   ; Window.blit d.w d.w.Window.raw 0 d.o (!o_pos + pre) rst )
              else Window.blit d.w d.w.Window.raw off d.o !o_pos len ) ;
          o_pos := !o_pos + len ;
          if d.l - len == 0 then jump := Length else d.l <- d.l - len
      done ;

      d.hold <- Nativeint.to_int !hold ;
      d.bits <- !bits ;
      d.i_pos <- !i_pos ;
      d.o_pos <- !o_pos ;
      d.jump <- !jump ;
      d.k <- slow_inflate lit dist !jump ; (* allocation *)
      d.s <- Slow ;

      if i_rem d > 0 then Flush else ( match d.src with
          | `String _ -> eoi d ; K
          (* XXX(dinosaure): [K] is required here mostly because the semantic of
             the hot-loop. If we reach end of input, we may have some trailing
             bits in [d.hold] and we need to process them.

             [slow_inflate] is more precise (but... slow) and will consume them
             to reach [End_of_inflate] then correctly. *)
          | `Channel _ -> assert false (* TODO *)
          | `Manual -> Await )
    with End ->
      d.hold <- Nativeint.to_int !hold ;
      d.bits <- !bits ;
      d.i_pos <- !i_pos ;
      d.o_pos <- !o_pos ;

      if d.last
      then ( d.s <- End_of_inflate
           ; End )
      else ( d.s <- Header
           ; K )
       | Invalid_distance -> err_invalid_distance d

  let fixed d =
    let lit, dist = fixed_lit, fixed_dist in
    d.literal <- lit ;
    d.distance <- dist ;
    d.jump <- Length ;
    d.s <- Inflate ; (* allocation *)
    inflate lit dist Length d

  (* XXX(dinosaure): [huffman] can raise an exception. *)
  let make_table t hlit hdist d =
    try
      if t.(256) == 0 then ( raise_notrace Invalid_huffman ) ;
      (* XXX(dinosaure): an huffman tree MUST have at least an End-Of-Block
         symbol. *)

      let t_lit, l_lit = huffman LENS t 0 hlit in
      let t_dist, l_dist = huffman DISTS t hlit hdist in

      let lit = Lookup.make t_lit l_lit in
      let dist = Lookup.make t_dist l_dist in

      d.literal <- lit ;
      d.distance <- dist ;
      d.jump <- Length ;
      d.s <- Inflate ; (* allocation *)
      inflate lit dist Length d
    with Invalid_huffman ->
      err_invalid_dictionary d

  let inflate_table d =
    let[@warning "-8"] Inflate_table { t; l= max_bits; r= res; h= (hlit, hdist, _) } = d.s in
    let max_res = hlit + hdist in
    let mask = (1 lsl max_bits) - 1 in
    let get k d =
      let len, v =
        t.(d.hold land mask) lsr 15,
        t.(d.hold land mask) land ((1 lsl 15) - 1) in
      d.hold <- d.hold lsr len ;
      d.bits <- d.bits - len ;
      k v d in
    let get k d = c_peek_bits max_bits (get k) d in
    let get_bits n k d =
      let k d =
        let v = d.hold land ((1 lsl n) - 1) in
        d.hold <- d.hold lsr n ;
        d.bits <- d.bits - n ;
        k v d in
      c_peek_bits n k d in
    let ret r d =
      make_table r hlit hdist d in
    (* XXX(dinosaure): [prv] and [i] are stored as associated env of [go]. We
       can not retake them from [d.s]. *)
    let rec record i copy len d =
      if i + copy > max_res then err_invalid_dictionary d
      else ( for x = 0 to copy - 1 do res.(i + x) <- len done
           ; if i + copy < max_res then get (fun d -> go (i + copy) d) d else ret res d )
    and go i v d =
      if v < 16
      then ( res.(i) <- v
           ; if succ i < max_res then get (fun d -> go (succ i) d) d else ret res d )
      else if v == 16
      then ( let k v d = record i (v + 3) res.(i - 1) d in
             if i == 0 then err_invalid_dictionary d else get_bits 2 k d )
      else if v == 17
      then ( let k v d = record i (v + 3) 0 d in
             get_bits 3 k d )
      else if v == 18
      then ( let k v d = record i (v + 11) 0 d in
             get_bits 7 k d )
      else assert false (* TODO: really never occur? *) in
    let k v d = go 0 v d in
    get k d

  let table d =
    let[@warning "-8"] Table { hlit; hdist; hclen; } = d.s in
    let hold = ref d.hold in
    let bits = ref d.bits in
    let i = ref 0 in

    let res = Array.make 19 0 in

    while !i < hclen
    do
      let code = !hold land 0x7 in
      res.(zigzag.(!i)) <- code ;
      hold := !hold lsr 3 ;
      bits := !bits - 3 ;
      incr i ;
    done ;

    try
      let t, l = huffman CODES res 0 19 in

      d.hold <- !hold ;
      d.bits <- !bits ;
      d.s <- Inflate_table { t; l
                           ; r= Array.make (hlit + hdist) 0
                           ; h= (hlit, hdist, hclen) } ;
      inflate_table d
    with Invalid_huffman ->
      err_invalid_dictionary d

  let dynamic d =
    let l_header d =
      let hlit = (d.hold land 0x1f) + 257 in
      let hdist = ((d.hold land 0x3e0) lsr 5) + 1 in
      let hclen = ((d.hold land 0x3c00) lsr 10) + 4 in

      d.s <- Table { hlit; hdist; hclen; } ;
      d.hold <- d.hold lsr 14 ;
      d.bits <- d.bits - 14 ;
      c_peek_bits (hclen * 3) table d in
    c_peek_bits 14 l_header d

  let decode_k d = match d.s with
    | Header ->
      (* XXX(dinosaure): check this code, we should need a [k]ontinuation. *)
      let l_header d =
        assert (d.bits >= 3) ; (* allocation *)

        let last = d.hold land 1 == 1 in
        let k =
          match (d.hold land 0x6) asr 1 with
          | 0 -> flat_header
          | 1 -> fixed
          | 2 -> dynamic
          | 3 -> err_invalid_kind_of_block
          | _ -> assert false in
        d.last <- last ;
        d.hold <- d.hold lsr 3 ;
        d.bits <- d.bits - 3 ;
        d.k <- k ;
        k d in
      c_peek_bits 3 l_header d
    | Table { hclen; _ } -> c_peek_bits (hclen * 3) table d
    | Inflate_table _ -> d.k d
    | Inflate ->
      if i_rem d > 1
      then inflate d.literal d.distance d.jump d
      else ( d.s <- Slow ; slow_inflate d.literal d.distance d.jump d )
    | Slow -> d.k d
    | Flat_header -> d.k d
    | Flat -> flat d
    | Checkseum -> d.k d
    | End_of_inflate -> End

  let rec decode d = match decode_k d with
    | Await -> `Await
    | Flush -> `Flush
    | End -> `End
    | Malformed err -> `Malformed err
    | K -> decode d

  let dst_rem d = bigstring_length d.o - d.o_pos
  let flush d = d.o_pos <- 0

  let decoder src ~o ~w =
    let i, i_pos, i_len = match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    { src
    ; i
    ; i_pos
    ; i_len
    ; o
    ; o_pos= 0
    ; hold= 0
    ; bits= 0
    ; last= false
    ; l= 0
    ; d= 0
    ; literal= fixed_lit
    ; distance= fixed_dist
    ; jump= Length
    ; w= Window.from w
    ; s= Header
    ; k= decode_k }
end

module T = struct
  module Heap = struct
    type t = { heap : int array
             ; mutable len : int
             ; mutable max : int }

    let make () =
      { heap= Array.make _heap_size 0
      ; len= 0
      ; max= _heap_size }

    let populate ~length ~freqs tree_lengths ~depth heap =
      (* assert (Array.length tree_lengths = Array.length freqs) ;
         assert (Array.length depth = heap.max) ;
         assert (Array.length heap.heap = heap.max) ;
         assert (heap.max = _heap_size) ;
      *)
      let max_code = ref (-1) in

      (* Construct the initial heap, with least frequent element in
         heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1].
         heap[0] is not used. *)
      for n = 0 to length - 1
      do
        if freqs.(n) <> 0
        then
          ( heap.len <- heap.len + 1
          ; heap.heap.(heap.len) <- n
          ; max_code := n
          ; depth.(n) <- 0 )
        else tree_lengths.(n) <- 0
        (* XXX(dinosaure): we consider that [tree_lengths] can have bad
           informations, so we clean it. However, it was initialized with [0] in
           [T.make] builder. *)
      done ;

      !max_code

    (* The pkzip format requires that at least one distance code exists,
       and that at least one bit should be sent even if there is only one
       possible code. So to avoid special checks later on we force at least
       two codes of non zero frequency. *)
    let pkzip max_code ~freqs ~depth heap =
      let max_code = ref max_code in

      while heap.len < 2 do
        let node = if !max_code < 2 then ( incr max_code ; !max_code ) else 0 in
        freqs.(node) <- 1 ;
        heap.len <- heap.len + 1 ;
        heap.heap.(heap.len) <- node ;
        depth.(node) <- 0 ;
      done ;

      !max_code

    let[@inline] smaller freqs n m depth =
      (freqs.(n) < freqs.(m) || (freqs.(n) = freqs.(m) && depth.(n) <= depth.(m)))

    let pqdownheap ~freqs ~depth heap k =
      let exception Break in
      let v = heap.heap.(k) in
      let j = ref (k lsl 1) in
      let k = ref k in

      ( try while !j <= heap.len do
        if !j < heap.len
           && smaller freqs heap.heap.(!j+1) heap.heap.(!j) depth
        then incr j ;
        if smaller freqs v heap.heap.(!j) depth
        then raise_notrace Break ;
        heap.heap.(!k) <- heap.heap.(!j) ;
        k := !j ;
        j := !j lsl 1 ;
      done with Break -> () ) ;

      heap.heap.(!k) <- v

    let pqremove ~freqs ~depth heap =
      let top = heap.heap.(_smallest) in
      heap.heap.(_smallest) <- heap.heap.(heap.len) ;
      heap.len <- heap.len - 1 ;
      pqdownheap ~freqs ~depth heap _smallest ; top
  end

  (* Reverse the first len bits of a code, using a straightforward code
     (a faster method would use a table). *)
  let reverse_code code len =
    (* assert (1 <= len && len <= 15); *)
    let res = ref 0 in
    let len = ref len in
    let code = ref code in
    while
      res := !res lor (!code land 1) ;
      code := !code asr 1 ;
      res := !res lsl 1 ;
      decr len ; !len > 0 do () done ;
    !res asr 1

  let is_graph = function
    | '\032' .. '\126' -> true
    | _ -> false

  let generate_codes ~tree_lengths ~max_code ~bl_count =
    let tree_codes = Array.make (Array.length tree_lengths) 0 in
    let next_code = Array.make (_max_bits + 1) 0 in
    let code = ref 0 in

    (* The distribution counts are fist used to generate the code values without
       bit reversal. *)
    for bits = 1 to _max_bits
    do
      code := (!code + bl_count.(bits - 1)) lsl 1 ;
      next_code.(bits) <- (!code land 0xffff) ;
    done ;

    (* check that the bit counts in [bl_count] are consistent. The last code
       must be all ones. *)
    assert (!code + bl_count.(_max_bits) - 1 = (1 lsl _max_bits) - 1);

    for n = 0 to max_code
    do
      let len = tree_lengths.(n) in
      if len > 0
      then
        (* Now reverse the bits. *)
        ( tree_codes.(n) <- reverse_code next_code.(len) len
        ; next_code.(len) <- next_code.(len) + 1 )
    done ;

    tree_codes

  let generate_lengths ~tree_dads ~tree_lengths ~max_code ~max_length heap ~bl_count =
    (* assert (Array.length bl_count = _max_bits + 1) ;
       assert (Array.for_all ((=) 0) bl_count) ;
    *)

    (* In a first pass, compute the optimal bit lengths (which may overflow in
       the case of the bit length tree). *)
    tree_lengths.(heap.Heap.heap.(heap.max)) <- 0 ; (* root of the heap. *)
    let overflow = ref 0 in

    Array.fill bl_count 0 (Array.length bl_count) 0 ;

    for h = heap.max + 1 to _heap_size - 1
    do
      let n = heap.heap.(h) in
      let bits = tree_lengths.(tree_dads.(n)) + 1 in
      let bits = if bits > max_length then ( incr overflow ; max_length ) else bits in
      tree_lengths.(n) <- bits ;

      if n <= max_code (* XXX(dinosaure): it's a leaf. *)
      then ( bl_count.(bits) <- bl_count.(bits) + 1 )
    done ;

    if !overflow != 0 (* This happends for example on obj2 and pic of the Calgary corpus. *)
    then
      ( let rec go () =
          let bits = ref (max_length - 1) in
          while bl_count.(!bits) == 0 do decr bits done ;
          bl_count.(!bits) <- bl_count.(!bits) - 1 ;
          bl_count.(!bits + 1) <- bl_count.(!bits + 1) + 2 ;
          bl_count.(max_length) <- bl_count.(max_length) - 1 ;

          overflow := !overflow - 2 ;

          if !overflow > 0 then go () in

        go () ;

        let h = ref _heap_size in
        for bits = max_length downto 1
        do
          let n = ref bl_count.(bits) in

          while !n != 0 do
            decr h ;
            let m = heap.heap.(!h) in
            if m <= max_code
            then ( if tree_lengths.(m) <> bits
                   then ( tree_lengths.(m) <- bits )
                 ; decr n )
          done
        done )

  type tree =
    { lengths : int array
    ; max_code : int
    ; tree : Lookup.t }

  let pp_tree ppf t =
    Fmt.pf ppf "{ @[<hov>lengths= @[<hov>%a@];@ \
                         max_code= %d;@ \
                         tree= @[<hov>%a@];@] }"
      Fmt.(Dump.array int) t.lengths
      t.max_code Lookup.pp t.tree

  let make ~length ?(max_length= _max_bits) freqs ~bl_count =
    let heap = Heap.make () in
    let depth = Array.make (2 * _l_codes + 1) 0 in
    let tree_dads = Array.make _heap_size 0 in
    let tree_lengths = Array.make _heap_size 0 in

    let max_code = Heap.populate ~length ~freqs ~depth tree_lengths heap in
    let max_code = Heap.pkzip max_code ~freqs ~depth heap in

    for n = heap.len / 2 downto 1 do Heap.pqdownheap ~freqs ~depth heap n done ;

    let node = ref length in

    let rec go () =
      let n = Heap.pqremove ~freqs ~depth heap in
      let m = heap.heap.(_smallest) in

      heap.max <- heap.max - 1 ;
      heap.heap.(heap.max) <- n ;
      heap.max <- heap.max - 1 ;
      heap.heap.(heap.max) <- m ;

      freqs.(!node) <- freqs.(n) + freqs.(m) ;
      depth.(!node) <- (if depth.(n) >= depth.(m) then depth.(n) else depth.(m)) + 1 ;
      tree_dads.(n) <- !node ;
      tree_dads.(m) <- !node ;
      heap.heap.(_smallest) <- !node ;
      incr node ;
      Heap.pqdownheap ~freqs ~depth heap _smallest ;

      if heap.len >= 2 then go ()
      else ( heap.max <- heap.max - 1
           ; heap.heap.(heap.max) <- heap.heap.(_smallest) ) in

    go () ;
    generate_lengths ~tree_dads ~tree_lengths ~max_code ~max_length heap ~bl_count ;
    let tree_codes = generate_codes ~tree_lengths ~max_code ~bl_count in
    let length = ref 0 in

    let tree =
      Array.map2 (fun len code ->
        length := max !length len ;
        ((len lsl _max_bits) lor code))
        tree_lengths tree_codes in
    { lengths= tree_lengths
    ; max_code
    ; tree= { Lookup.t= tree
            ; m= (1 lsl !length) - 1
            ; l= !length } }

  let scan tree_lengths max_code ~bl_freqs  =
    let prevlen = ref (-1) in
    let nextlen = ref tree_lengths.(0) in
    let curlen = ref !nextlen in

    let count = ref 0 in

    let max_count = ref 7 in
    let min_count = ref 4 in

    let exception Continue in

    if !nextlen = 0 then ( max_count := 138 ; min_count := 3 ) ;
    tree_lengths.(max_code + 1) <- 0xffff ;

    for n = 0 to max_code do
      curlen := !nextlen ;
      nextlen := tree_lengths.(n + 1) ;
      incr count ;

      try
        if !count < !max_count && !curlen == !nextlen
        then raise_notrace Continue
        else if !count < !min_count
        then bl_freqs.(!curlen) <- bl_freqs.(!curlen) + !count
        else if !curlen != 0
        then ( if !curlen != !prevlen then bl_freqs.(!curlen) <- bl_freqs.(!curlen) + 1
             ; bl_freqs.(_rep_3_6) <- bl_freqs.(_rep_3_6) + 1 )
        else if !count <= 10
        then bl_freqs.(_repz_3_10) <- bl_freqs.(_repz_3_10) + 1
        else bl_freqs.(_repz_11_138) <- bl_freqs.(_repz_11_138) + 1 ;

        count := 0 ;
        prevlen := !curlen ;

        if !nextlen == 0
        then ( max_count := 138 ; min_count := 3 )
        else if !curlen = !nextlen
        then ( max_count := 6 ; min_count := 3 )
        else ( max_count := 7 ; min_count := 4 )
      with Continue -> ()
    done

  let code code lookup = lookup.Lookup.t.(code)
  let bits code len = (len lsl _max_bits) lor code

  let symbols i tree_lengths max_code ~bl_symbols ~bltree =
    let i = ref i in

    let prevlen = ref (-1) in
    let nextlen = ref tree_lengths.(0) in
    let curlen = ref !nextlen in

    let count = ref 0 in

    let max_count = ref 7 in
    let min_count = ref 4 in

    let exception Continue in

    if !nextlen = 0 then ( max_count := 138 ; min_count := 3 ) ;

    for n = 0 to max_code do
      curlen := !nextlen ;
      nextlen := tree_lengths.(n + 1) ;
      incr count ;

      try
        if !count < !max_count && !curlen == !nextlen
        then raise_notrace Continue
        else if !count < !min_count
        then while bl_symbols.(!i) <- code !curlen bltree.tree ; incr i ; decr count ; !count != 0 do () done
        else if !curlen != 0
        then
          ( if !curlen != !prevlen then ( bl_symbols.(!i) <- code !curlen bltree.tree ; incr i ; decr count )
          ; bl_symbols.(!i) <- code _rep_3_6 bltree.tree ; incr i
          ; bl_symbols.(!i) <- bits (!count - 3) 2 ; incr i )
        else if !count <= 10
        then
          ( bl_symbols.(!i) <- code _repz_3_10 bltree.tree ; incr i
          ; bl_symbols.(!i) <- bits (!count - 3) 3 ; incr i )
        else
          ( bl_symbols.(!i) <- code _repz_11_138 bltree.tree ; incr i
          ; bl_symbols.(!i) <- bits (!count - 11) 7 ; incr i ) ;

        count := 0 ;
        prevlen := !curlen ;

        if !nextlen == 0
        then ( max_count := 138 ; min_count := 3 )
        else if !curlen == !nextlen
        then ( max_count := 6 ; min_count := 3 )
        else ( max_count := 7 ; min_count := 4 )
      with Continue -> ()
    done ;

    !i
end

module B = struct
  type cmd = int
  type buf = (cmd, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t =
    { buf : buf
    ; mutable w : int
    ; mutable r : int
    ; mutable c : int }

  let mask t v = v land (t.c - 1)
  let empty t = t.r = t.w
  let size t = t.w - t.r
  let available t = t.c - (t.w - t.r)
  let full t = size t = t.c
  let length t = size t

  let is_empty t = empty t
  let is_full t = full t

  external unsafe_get : buf -> int -> int = "%caml_ba_ref_1"
  external unsafe_set : buf -> int -> int -> unit = "%caml_ba_set_1"

  exception Full
  exception Empty

  let push_exn t v =
    if (full [@inlined]) t then raise Full ;
    unsafe_set t.buf ((mask [@inlined]) t t.w) v ;
    t.w <- t.w + 1

  let pop_exn t =
    if (empty [@inlined]) t then raise Empty ;
    let r = unsafe_get t.buf ((mask [@inlined]) t t.r) in
    t.r <- t.r + 1 ; r

  let peek_exn t =
    if (empty [@inlined]) t then raise Empty ;
    unsafe_get t.buf ((mask [@inlined]) t t.r)

  let unsafe_junk t = t.r <- t.r + 1

  let junk_exn t n =
    if (size [@inlined]) t < n then Fmt.invalid_arg "You want to junk more than what we have" ;
    t.r <- t.r + n

  let copy ~off ~len : cmd =
    assert (len >= 3 && len <= 255 + 3) ;
    assert (off >= 1 && off <= 32767 + 1) ;
    ((len - 3) lsl 16) lor (off - 1) lor 0x2000000 [@@inline]

  let literal chr = Char.code chr [@@inline]
  let eob = 256

  let cmd = function
    | `Literal chr -> literal chr
    | `Copy (off, len) -> copy ~off ~len
    | `End -> 256

  let code cmd = match cmd land 0x2000000 <> 0 with
    | false ->
      if cmd == 256 then `End else `Literal (Char.chr (cmd land 0xff))
    | true ->
      let off = (cmd land 0xffff) + 1 in
      let len = ((cmd lsr 16) land 0x1ff) + 3 in (* XXX(dinosaure): ((1 lsl 9) - 1) - 0xff *)
      `Copy (off, len)

  let blit t buf off len =
    if available t < len then raise Full ;
    let msk = mask t t.w in
    let pre = t.c - msk in
    let rst = len - pre in
    if rst > 0
    then ( for i = 0 to pre - 1 do unsafe_set t.buf (msk + i) (unsafe_get_uint8 buf (off + i)) done ;
           for i = 0 to rst - 1 do unsafe_set t.buf i (unsafe_get_uint8 buf (off + pre + i)) done )
    else for i = 0 to len - 1 do unsafe_set t.buf (msk + i) (unsafe_get_uint8 buf (off + i)) done ;
    t.w <- t.w + len

  let create length =
    { buf= Bigarray.Array1.create Bigarray.int Bigarray.c_layout length
    ; w= 0
    ; r= 0
    ; c= length }

  let reset t =
    t.w <- 0 ; t.r <- 0

  let to_list t =
    let res = ref [] in
    let len = size t in
    let msk = mask t t.r in
    let pre = t.c - msk in
    let rst = len - pre in

    if rst > 0
    then ( for i = 0 to pre - 1 do res := code (unsafe_get t.buf i) :: !res done
         ; for i = 0 to rst - 1 do res := code (unsafe_get t.buf i) :: !res done )
    else for i = 0 to len - 1 do res := code (unsafe_get t.buf i) :: !res done ;

    List.rev !res

  let ( <.> ) f g = fun x -> f (g x)

  let of_list lst =
    let q = create 4096 in (* TODO *)
    List.iter (push_exn q <.> cmd) lst ; q
end

module N = struct
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type dynamic =
    { ltree : T.tree
    ; dtree : T.tree
    ; bltree : T.tree
    ; h_lit : int
    ; h_dst : int
    ; h_len : int
    ; symbols : int array }

  let pp_dynamic ppf d =
    Fmt.pf ppf "{ @[<hov>ltree= @[<hov>%a@];@ \
                         dtree= @[<hov>%a@];@ \
                         bltree= @[<hov>%a@];@ \
                         h_lit= %d;@ \
                         h_dst= %d;@ \
                         h_len= %d;@ \
                symbols= @[<hov>%a@];@] }"
      T.pp_tree d.ltree
      T.pp_tree d.dtree
      T.pp_tree d.bltree
      d.h_lit d.h_dst d.h_len
      Fmt.(Dump.array int) d.symbols

  type literals = int array
  type distances = int array

  let pp_literals = Fmt.(Dump.array int)
  let pp_distances = Fmt.(Dump.array int)
  external unsafe_literals_to_array : literals -> int array = "%identity"
  external unsafe_distances_to_array : distances -> int array = "%identity"

  let make_literals () =
    let res = Array.make (2 * _l_codes + 1) 0 in
    res.(256) <- 1 ; res

  let succ_literal literals chr =
    literals.(Char.code chr) <- literals.(Char.code chr) + 1
  let succ_length literals length =
    assert (length >= 3 && length <= 255 + 3) ;
    literals.(256 + 1 + _length.(length - 3)) <- literals.(256 + 1 + _length.(length - 3)) + 1

  let make_distances () = Array.make (2 * _d_codes + 1) 0

  let succ_distance distances distance =
    assert (distance >= 1 && distance <= 32767 + 1) ;
    distances.(_distance (pred distance)) <- distances.(_distance (pred distance)) + 1

  let bl_tree ltree dtree ~bl_count =
    let bl_freqs = Array.make (2 * _bl_codes + 1) 0 in
    T.scan ltree.T.lengths ltree.T.max_code ~bl_freqs ;
    T.scan dtree.T.lengths dtree.T.max_code ~bl_freqs ;

    let bltree = T.make ~length:_bl_codes ~max_length:7 bl_freqs ~bl_count in
    (* XXX(dinosaure): [T.make] needs [max_length] to avoid generation of a bad bltree (limited to 7 bits with extra). *)
    let max_blindex = ref (_bl_codes - 1) in
    let exception Break in

    ( try while !max_blindex >= 3 do
          if bltree.T.lengths.(zigzag.(!max_blindex)) <> 0
          then raise_notrace Break ;
          decr max_blindex
        done
      with Break -> () ) ;

    !max_blindex, bltree

  let dynamic_of_frequencies
    : literals:int array -> distances:int array -> dynamic
    = fun ~literals:lit_freqs ~distances:dst_freqs ->
    let bl_count = Array.make (_max_bits + 1) 0 in
    let ltree = T.make ~length:_l_codes lit_freqs ~bl_count in
    let dtree = T.make ~length:_d_codes dst_freqs ~bl_count in
    let max_blindex, bltree = bl_tree ltree dtree ~bl_count in
    let bl_symbols = Array.make (_l_codes + _d_codes) 0 in
    let i = T.symbols 0 ltree.T.lengths ltree.T.max_code ~bltree ~bl_symbols in
    let i = T.symbols i dtree.T.lengths dtree.T.max_code ~bltree ~bl_symbols in
    let bl_symbols = Array.sub bl_symbols 0 i in

    { h_lit= ltree.T.max_code + 1
    ; h_dst= dtree.T.max_code + 1
    ; h_len= max_blindex + 1
    ; bltree
    ; ltree
    ; dtree
    ; symbols= bl_symbols }

  let invalid_encode () = Fmt.invalid_arg "expected `Await encode"

  let unsafe_blit src src_off dst dst_off len =
    let a = bigstring_sub src src_off len in
    let b = bigstring_sub dst dst_off len in
    bigstring_blit a b

  type kind = Flat of int | Fixed | Dynamic of dynamic
  type block = { kind: kind; last: bool; }

  type encode = [ `Await | `Flush | `Block of block ]

  let exists v block = match v, block.kind with
    | (`Copy _ | `End), Flat _ ->
      Fmt.invalid_arg "copy code in flat block can not exist"
    | `Literal chr, Dynamic dynamic ->
      dynamic.ltree.T.tree.Lookup.t.(Char.code chr) lsr _max_bits > 0
    | `Copy (off, len), Dynamic dynamic ->
      dynamic.ltree.T.tree.Lookup.t.(256 + 1 + _length.(len - 3)) lsr _max_bits > 0
      && dynamic.dtree.T.tree.Lookup.t.(_distance (pred off)) lsr _max_bits > 0
    | `End, (Fixed | Dynamic _) | `Literal _, (Flat _ | Fixed) | `Copy _, Fixed -> true

  type encoder =
    { dst : dst
    ; mutable blk : block
    ; mutable hold : int
    ; mutable bits : int
    ; mutable bits_rem : [ `Rem of int | `Pending ]
    ; mutable o : bigstring
    ; mutable o_pos : int
    ; mutable o_max : int
    ; b : B.t
    ; mutable k : encoder -> encode -> [ `Ok | `Partial | `Block ] }

  (* remaining bytes to write in [e.o]. *)
  let o_rem e = e.o_max - e.o_pos + 1

  (* set [e.o] with [s]. *)
  let dst e s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s) then invalid_bounds j l ;
    e.o <- s ;
    e.o_pos <- j ;
    e.o_max <- j + l - 1

  let partial k e = function
    | `Await -> k e
    (* if [encode] returns [`Partial], end-user must call [encode] with
       [`Await] value. Otherwise, it's a bad logic. *)
    | `Literal _ | `Copy _ | `Block _
    | `Flush | `End -> invalid_encode ()

  let nothing _ = function
    | `Await | `Flush | `End
    | `Literal _ | `Copy _ | `Block _ ->
      Fmt.invalid_arg "You encoded your last block, encoder has nothing to do more"

  let flush k e = match e.dst with
    | `Manual -> e.k <- partial k ; `Partial
    | `Channel oc ->
      output_bigstring oc e.o 0 e.o_pos ; e.o_pos <- 0 ; k e
    | `Buffer b ->
      for i = 0 to e.o_pos - 1
      do Buffer.add_char b (Char.unsafe_chr (unsafe_get_uint8 e.o i)) done ;
      (* TODO: check why we need [unsafe_chr]. *)
      e.o_pos <- 0 ; k e

  let pp_chr = Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

  let rec c_byte byte k e =
    let rem = o_rem e in
    if rem < 1
    then flush (fun e -> c_byte byte k e) e
    else
      ( unsafe_set_uint8 e.o e.o_pos byte
      ; e.o_pos <- e.o_pos + 1
      ; k e )

  let rec c_short short k e =
    let rem = o_rem e in
    if rem < 2
    then flush (fun e -> c_short short k e) e
    else
      ( unsafe_set_uint16 e.o e.o_pos short
      ; e.o_pos <- e.o_pos + 2
      ; k e )

  let c_bits bits long k e =
    if e.bits + long < 16
    then ( e.hold <- (bits lsl e.bits) lor e.hold
         ; e.bits <- e.bits + long
         ; k e )
    else
      ( let k e =
          e.hold <- e.hold lsr 16 ;
          e.bits <- e.bits - 16 ;
          k e in
        e.hold <- (bits lsl e.bits) lor e.hold
      ; e.bits <- e.bits + long
      ; c_short (e.hold land 0xffff) k e )

  (* encode dynamic huffman tree *)

  let encode_huffman dynamic k e =
    let flush e =
      if e.bits >= 16
      then ( let k e =
                e.hold <- e.hold lsr 16 ;
                e.bits <- e.bits - 16 ;
                k e in
              c_short (e.hold land 0xffff) k e )
      else if e.bits >= 8
      then ( let k e =
                e.hold <- e.hold lsr 8 ;
                e.bits <- e.bits - 8 ;
                k e in
              c_byte (e.hold land 0xff) k e )
       else k e in
    let rec go rank e =
      if rank == Array.length dynamic.symbols
      then flush e
      else
        let len, code =
          dynamic.symbols.(rank) lsr _max_bits,
          dynamic.symbols.(rank) land ((1 lsl _max_bits) - 1) in
        (* max_len: 7 *)
        c_bits code len (go (succ rank)) e in
    go 0 e

  let encode_zigzag dynamic k e =
    let rec go rank e =
      if rank == dynamic.h_len
      then encode_huffman dynamic k e
      else
        let k e = go (succ rank) e in
        c_bits dynamic.bltree.T.lengths.(zigzag.(rank)) 3 k e in
    go 0 e

  let encode_dynamic_header last dynamic k e =
    let k5 e = encode_zigzag dynamic k e in
    let k4 e = c_bits (dynamic.h_len - 4) 4 k5 e in
    let k3 e = c_bits (dynamic.h_dst - 1) 5 k4 e in
    let k2 e = c_bits (dynamic.h_lit - 257) 5 k3 e in
    let k1 e = c_bits 0x2 2 k2 e in
    let k0 e = c_bits (if last then 1 else 0) 1 k1 e in

    k0 e

  let encode_fixed_header last k e =
    let k1 e = c_bits 0x1 2 k e in
    let k0 e = c_bits (if last then 1 else 0) 1 k1 e in

    k0 e

  let pending_bits k e =
    assert (e.bits <= 16) ;
    let k = flush k in

    if e.bits > 8
    then ( let k e =
              e.hold <- 0 ;
              e.bits_rem <- `Rem (16 - e.bits) ;
              e.bits <- 0 ;
              k e in
            c_short (e.hold land 0xffff) k e )
    else if e.bits > 0
    then ( let k e =
              e.hold <- 0 ;
              e.bits_rem <- `Rem (8 - e.bits) ;
              e.bits <- 0 ;
              k e in
            c_byte (e.hold land 0xff) k e )
    else k e

  let pp_code ppf = function
    | `Literal chr -> Fmt.pf ppf "(`Literal %02x:%a)" (Char.code chr) pp_chr chr
    | `Copy (off, len) -> Fmt.pf ppf "(`Copy off:%d len:%d)" off len
    | `End -> Fmt.string ppf "`End"

  let rec block e = function
    | `Await ->
      e.k <- block ; `Ok
    | `Flush ->
      let k e = e.k <- block ; `Ok in
      flush k e
    | `Block block ->
      let k e v = match block.kind with
        | Dynamic dynamic -> encode_dynamic_header block.last dynamic (fun e -> e.k <- encode ; e.k e v) e
        | Fixed -> encode_fixed_header e.blk.last (fun e -> e.k <- encode ; e.k e v) e
        | _ -> assert false in
      e.blk <- block ; e.k <- k ; `Ok
    | (`Literal _ | `Copy _ | `End) as v ->
      assert (e.blk.last = false) ;

      match e.blk.kind with
      | Dynamic dynamic -> encode_dynamic_header e.blk.last dynamic (fun e -> e.k <- encode ; e.k e v) e
      | Fixed -> encode_fixed_header e.blk.last (fun e -> e.k <- encode ; e.k e v) e
      | _ -> assert false (* TODO *)

  and write k e =
    let o_pos = ref e.o_pos in
    let hold = ref e.hold in
    let bits = ref e.bits in

    let exception End in

    let emit e =
      if !bits >= 16
      then ( unsafe_set_uint16 e.o !o_pos !hold
           ; hold := !hold lsr 16
           ; bits := !bits - 16
           ; o_pos := !o_pos + 2 ) in

    let ltree, dtree = match e.blk with
      | { kind= Dynamic dynamic; _ } ->
        dynamic.ltree.T.tree, dynamic.dtree.T.tree
      | { kind= Fixed; _ } ->
        _static_ltree, _static_dtree
      | _ -> assert false in

    try while e.o_max - !o_pos + 1 > 1 && not (B.is_empty e.b) do
        let cmd = B.peek_exn e.b in

        if not (exists (B.code cmd) e.blk)
        then raise_notrace End ;

        B.unsafe_junk e.b ;

        match cmd land 0x2000000 == 0 with
        | true ->
          let len, v = Lookup.get ltree cmd in
          hold := (v lsl !bits) lor !hold ;
          bits := !bits + len ;
          emit e
        | false ->
          let off, len = cmd land 0xffff, (cmd lsr 16) land 0x1ff in

          let code = _length.(len) in
          let len0, v0 = Lookup.get ltree (code + 256 + 1) in
          (* len0_max: 15 *)
          let len1, v1 = _extra_lbits.(code), len - _base_length.(code)  in
          (* len1_max: 5 *)

          let code = _distance off in
          let len2, v2 = Lookup.get dtree code in
          (* len2_max: 15 *)
          let len3, v3 = _extra_dbits.(code), off - _base_dist.(code) in
          (* len3_max: 13 *)

          hold :=
                  (v3 lsl (!bits + len0 + len1 + len2))
              lor (v2 lsl (!bits + len0 + len1))
              lor (v1 lsl (!bits + len0))
              lor (v0 lsl !bits)
              lor !hold ;
          bits := !bits + len0 + len1 + len2 + len3 ;
          (* len_max: 48 *)
          emit e
      done ;

      e.hold <- !hold ;
      e.bits <- !bits ;
      e.o_pos <- !o_pos ;

      (* XXX(dinosaure): at least we need 2 bytes in any case. *)
      if o_rem e > 1 then k e else flush k e
    with End -> match e.blk with
      | { kind= Dynamic dynamic; _ } ->
        (* XXX(dinosaure): it's safe because we raise End only if [e.o_max - !o_pos + 1 > 1]. *)
        let len, v = Lookup.get dynamic.ltree.T.tree 256 in
        hold := (v lsl !bits) lor !hold ;
        bits := !bits + len ;
        emit e ;

        e.hold <- !hold ;
        e.bits <- !bits ;
        e.o_pos <- !o_pos ;

        let k = if e.blk.last then nothing else block in
        let k e = e.k <- k ; `End in
        if e.blk.last then pending_bits k e else k e
      | _ -> assert false (* TODO *)

  and encode_end k e =
    let rec pending_cmds e =
      if B.is_empty e.b then ( if e.blk.last then pending_bits k e else k e ) else write pending_cmds e
    and push_end e =
      if B.full e.b then write push_end e
      else ( B.push_exn e.b 256 ; pending_cmds e ) in
    push_end e

  and encode_cmd cmd k e =
    let rec push_cmd e =
      if B.full e.b then write push_cmd e
      else ( B.push_exn e.b cmd ; k e ) in
    push_cmd e

  and encode e = function
    | #code as v ->
      let k e = e.k <- encode ; `Ok in
      encode_cmd (B.cmd v) k e
    | `Await -> e.k <- encode ; `Ok (* XXX(dinosaure): do nothing. *)
    | `Flush ->
      let k e = e.k <- encode ; `Ok in
      let rec encode_cmds e =
        if B.is_empty e.b then k e else write encode_cmds e in
      encode_cmds e
    | `End ->
      let k e = e.k <- if e.blk.last then nothing else block ; `Ok in
      encode_end (fun e -> flush k e) e
    | `Block _ as v ->
      if e.blk.last
      then Fmt.invalid_arg "Impossible to make a new block when the current block is the last one"
      else
        let k e = block e v in
        encode_end (fun e -> flush k e) e

  let dst_rem = o_rem

  let bits_rem t = match t.bits_rem with
    | `Rem rem -> rem
    | `Pending -> Fmt.invalid_arg "Encoder does not reach EOB of last block"

  let encoder dst block ~q =
    let o, o_pos, o_max = match dst with
      | `Manual -> bigstring_empty, 1, 0
      | `Buffer _
      | `Channel _ -> bigstring_create io_buffer_size, 0, io_buffer_size - 1 in
    let k e v = match block.kind with
      | Dynamic dynamic -> encode_dynamic_header block.last dynamic (fun e -> e.k <- encode ; e.k e v) e
      | Fixed -> encode_fixed_header block.last (fun e -> e.k <- encode ; e.k e v) e
      | _ -> assert false in
    { dst
    ; blk= block
    ; hold= 0
    ; bits= 0
    ; bits_rem= `Pending
    ; o
    ; o_pos
    ; o_max
    ; b= q
    ; k }

  let encode e = e.k e
end

module W = struct
  type t =
    { raw : bigstring
    ; mutable r : int
    ; mutable w : int
    ; mutable c : Optint.t }

  let max = 1 lsl 15
  let mask = max - 1
  let ( = ) (a : int) b = a == b

  let mask v = v land mask
  let empty t = t.r = t.w
  let size t = t.w - t.r
  let available t = max - (t.w - t.r)
  let full t = size t = max
  let length t = size t

  let is_empty t = empty t

  let make () =
    { raw= Bigarray.Array1.create Bigarray.char Bigarray.c_layout max
    ; r= 0
    ; w= 0
    ; c= Checkseum.Adler32.default }

  let from raw =
    { raw; r= 0; w= 0; c= Checkseum.Adler32.default }

  let update w =
    let c = Checkseum.Adler32.unsafe_digest_bigstring w.raw 0 max w.c in
    w.c <- c

  let iter t f =
    let len = size t in
    let msk = mask t.r in
    let pre = max - msk in
    let rst = len - pre in

    if rst > 0
    then ( for i = 0 to pre - 1 do f (unsafe_get_char t.raw (msk + i)) done
         ; for i = 0 to rst - 1 do f (unsafe_get_char t.raw i) done )
    else for i = 0 to len - 1 do f (unsafe_get_char t.raw (msk + i)) done

  (* XXX(dinosaure): [unsafe_] means that [i] can be out of [r] and [w] - but
     still is a valid index in [raw]. *)
  let unsafe_get_char t i = unsafe_get_char t.raw (mask i) [@@inline always]

  let m = max - 1

  let unsafe_get_uint16 t i =
    if (mask i) == m
    then
      if Sys.big_endian
      then (unsafe_get_uint8 t.raw 0 lsl 8) lor unsafe_get_uint8 t.raw m
      else (unsafe_get_uint8 t.raw m lsl 8) lor unsafe_get_uint8 t.raw 0
    else unsafe_get_uint16 t.raw (mask i) [@@inline always]

  let unsafe_get_uint8 t i =
    unsafe_get_uint8 t.raw (mask i) [@@inline always]

  let e t r s =
    let r = mask r in
    let w = mask t.w in
    let s = mask s in
    r <= s && s < w

  let tail w =
    let msk = mask w.w in
    if msk > 0
    then ( let c = Checkseum.Adler32.unsafe_digest_bigstring w.raw 0 msk w.c in
           w.c <- c )

  let feed t buf off len =
    let msk = mask t.w in
    let pre = max - msk in
    let rst = len - pre in
    ( if rst > 0
      then ( unsafe_blit buf off t.raw msk pre
           ; update t
           ; unsafe_blit buf (off + pre) t.raw 0 rst )
      else unsafe_blit buf off t.raw msk len ) ;
    t.w <- t.w + len

  let junk t len = t.r <- t.r + len

  let checksum w = w.c
end

module L = struct
  type offset = int
  type length = int
  type decode = [ `Await | `Flush | `End ]

  type src = [ `Channel of in_channel | `String of string | `Manual ]

  let _max_match = 258
  let _min_match = 3
  let _lookahead = _max_match + _min_match + 1
  let _hash_bits = 15
  let _hash_shift = (_hash_bits + _min_match - 1) / _min_match
  let _hash_size = 1 lsl _hash_bits
  let _hash_mask = _hash_size - 1

  type state =
    { src : src
    ; mutable i : bigstring
    ; mutable i_pos : int
    ; mutable i_len : int
    ; l : N.literals
    ; d : N.distances
    ; w : W.t
    ; h : int array
    ; h_msk : int
    ; b : B.t
    ; mutable k : state -> decode }

  let literals s = s.l
  let distances s = s.d

  let eoi s =
    s.i <- bigstring_empty ;
    s.i_pos <- 0 ;
    s.i_len <- min_int

  (* set [s.i] with [s]. *)
  let src d s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    if (l == 0) then eoi d
    else
      ( d.i <- s
      ; d.i_pos <- j
      ; d.i_len <- j + l - 1 )

  let refill k s = match s.src with
    | `String _ ->
      eoi s ; k s
    | `Channel ic ->
      let res = input_bigstring ic s.i 0 (Bigstringaf.length s.i) in
      src s s.i 0 res ; k s
    | `Manual ->
      s.k <- k ; `Await

  let flush k s = s.k <- k ; `Flush

  (* remaining bytes to read [s.i]. *)
  let i_rem s = s.i_len - s.i_pos + 1
  [@@inline]

  let pp_chr = Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

  (* XXX(dinoaure): [lt] is not very safe! *)

  let rec nothing s = s.k <- nothing ; `End

  let rec pending s =
    let len = min (B.available s.b) (W.size s.w) in

    let msk = W.mask s.w.r in
    let pre = W.max - msk in
    let rst = len - pre in

    ( if rst > 0
      then ( B.blit s.b s.w.raw msk pre
           ; B.blit s.b s.w.raw 0 rst )
      else B.blit s.b s.w.raw msk len ) ;

    W.iter s.w (N.succ_literal s.l) ;
    W.junk s.w len ;

    if W.is_empty s.w
    then ( W.tail s.w ; s.k <- nothing ; `End )
    else flush pending s

  let rec fill s =
    let rem = i_rem s in

    if rem <= 0 then ( if rem < 0 then pending s else refill fill s )
    else
      let len = min (W.available s.w) rem in
      W.feed s.w s.i s.i_pos len ;
      s.i_pos <- s.i_pos + len ;

      let k s =
        if i_rem s == 0 then refill fill s
        else fill s in

      (* XXX(dinosaure): optimize this branch. TODO! *)
      if W.size s.w >= 2
      then ( if B.available s.b < 2
             then flush fill s
             else deffast k s )
      else refill fill s

  and deffast k s =
    assert (B.available s.b >= 2) ;
    assert (W.size s.w >= 2) ;
    assert (W.size s.w <= W.max) ;

    let i = ref s.w.r in
    let len = ref 0 in
    let dst = ref 0 in

    let exception Match in
    let exception Literal in

    (* XXX(dinosaure): prelude, a [match] is >= 3.  *)
    let chr = W.unsafe_get_char s.w !i in
    B.push_exn s.b (B.cmd (`Literal chr)) ; incr i ;
    N.succ_literal s.l chr ;
    let chr = W.unsafe_get_char s.w !i in
    B.push_exn s.b (B.cmd (`Literal (W.unsafe_get_char s.w !i))) ; incr i ;
    N.succ_literal s.l chr ;

    while s.w.w - !i >= 3 && not (B.is_full s.b) do
      try
        if W.unsafe_get_uint8 s.w !i == W.unsafe_get_uint8 s.w (!i - 1)
           && W.unsafe_get_uint16 s.w (!i - 1) == W.unsafe_get_uint16 s.w (!i + 1)
        then ( len := 3 ; dst := 1 ; raise_notrace Match ) ;

        let hash =
          let v = W.unsafe_get_uint16 s.w !i in
          W.unsafe_get_uint16 s.w (!i + 1) lxor v in
        let source = s.h.(hash land s.h_msk) in
        s.h.(hash land s.h_msk) <- !i ;

        dst := !i - source ; (* XXX(dinosaure): should be safe where [!i] is the newest indice in [w]. *)

        if !dst == 0 || !dst >= W.max (* XXX(dinosaure): deliver only valid distance *)
          || source + 3 - min_int >= !i - min_int (* XXX(dinosaure): [source] ∈ no emitted characters *)
          || source - min_int < s.w.r - min_int (* XXX(dinosaure): too old! *)
          || W.unsafe_get_uint16 s.w !i <> W.unsafe_get_uint16 s.w source
          || W.unsafe_get_uint16 s.w (!i + 1) <> W.unsafe_get_uint16 s.w (source + 1)
        then ( raise_notrace Literal ) ;

        len := 3 ;

        raise_notrace Match
      with
      | Literal ->
        let chr = W.unsafe_get_char s.w !i in
        B.push_exn s.b (B.cmd (`Literal chr)) ; incr i ;
        N.succ_literal s.l chr
      | Match ->
        if !dst == 1
        then
          ( let vv = W.unsafe_get_uint16 s.w !i in
            let v = W.unsafe_get_uint8 s.w !i in

            while !len + 2 <= _max_match
                  && s.w.w - (!i + !len + 2) > 0 (* XXX(dinosaure): stay under write cursor. *)
                  && W.unsafe_get_uint16 s.w (!i + !len) == vv
            do len := !len + 2 done ;

            if !len + 1 <= _max_match
            && s.w.w - (!i + !len + 1) > 0
            && W.unsafe_get_uint8 s.w (!i + !len) == v
            then incr len ;

            B.push_exn s.b (B.cmd (`Copy (!dst, !len))) ; i := !i + !len ;
            N.succ_length s.l !len ;
            N.succ_distance s.d !dst )
        else
          ( let source = !i - !dst in
            (* XXX(dinosaure): try to go furthermore. *)

            while !len + 2 <= _max_match
                  && source + !len + 2 - min_int < !i + !len + 2 - min_int (* XXX(dinosaure): stay outside non-emitted characters. *)
                  && s.w.w - (!i + !len + 2) > 0 (* XXX(dinosaure): stay under write cursor. *)
                  && W.unsafe_get_uint16 s.w (!i + !len) == W.unsafe_get_uint16 s.w (source + !len)
            do len := !len + 2 done ;

            if !len + 1 <= _max_match
            && source + !len + 1 - min_int < !i - !len + 1 - min_int
            && s.w.w - (!i + !len + 1) > 0
            && W.unsafe_get_uint8 s.w (!i + !len) == W.unsafe_get_uint8 s.w (source + !len)
            then incr len ;

            B.push_exn s.b (B.cmd (`Copy (!dst, !len))) ; i := !i + !len ;
            N.succ_length s.l !len ;
            N.succ_distance s.d !dst )
    done ;

    W.junk s.w (!i - s.w.r) ;

    if B.is_full s.b
    then flush k s
    else ( if i_rem s == 0 then refill fill s else k s )

  let compress s = s.k s

  let state src ~w ~q =
    let i, i_pos, i_len = match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    { src
    ; i
    ; i_pos
    ; i_len
    ; l= N.make_literals ()
    ; d= N.make_distances ()
    ; w= W.from w
    ; h= Array.make (1 lsl 8) 0
    ; h_msk= (1 lsl 8) - 1
    ; b= q
    ; k= fill }
end
