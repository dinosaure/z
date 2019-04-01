[@@@warning "-32-34-37"]

let () = Printexc.record_backtrace true

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l
let bigstring_length x = Bigarray.Array1.dim x [@@inline]
let bigstring_sub x off len = Bigarray.Array1.sub x off len [@@inline]
let bigstring_blit a b = Bigarray.Array1.blit a b [@@inline]
let bigstring_to_string = Bigstringaf.to_string
let bigstring_of_string x = Bigstringaf.of_string ~off:0 ~len:(String.length x) x

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_get_uint26 : bigstring -> int -> int = "%caml_ba_ref_1" (* TODO *)

external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_set_uint32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"

let output_bigstring _ _ _ _ = ()

let invalid_bounds off len = Fmt.invalid_arg "Out of bounds (off: %d, len: %d)" off len

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
    | None -> raise Empty
    | Node (_, _, left, None) -> left
    | Node (_, _, None, right) -> right
    | Node
        (_, _, (Node (lp, le, _, _) as left), (Node (rp, re, _, _) as right))
      ->
        if lp <= rp then Node (lp, le, remove left, right)
        else Node (rp, re, left, remove right)

  let take = function
    | None -> raise Empty
    | Node (p, e, _, _) as queue -> (p, e, remove queue)

  let rec pp pp_data ppf = function
    | None -> Fmt.string ppf "<none>"
    | Node (p, e, l, r) ->
      Fmt.pf ppf "(Node (priority: %d, e: %a, l: %a, r: %a))"
        p pp_data e (Fmt.hvbox (pp pp_data)) l (Fmt.hvbox (pp pp_data)) r
end

module Lookup = struct
  type t =
    { t : int array
    ; m : int
    ; l : int }

  let mask = (1 lsl 15) - 1
  let make t m = { t; m= (1 lsl m) - 1; l= m }

  let get t i =
    let v = t.t.(i) in v lsr 15, v land mask (* allocation *)
  [@@inline]

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>t = @[<hov>%a@];@ m = %x;@ l = %d;@] }"
      Fmt.(Dump.array int) t.t t.m t.l
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

  let have t = t.w land max
  (* XXX(dinosaure): this is wrong. It's true for the first run, but then,
     should be equal to [1 lsl 15] evry times. *)

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

  type kind =
    | CODES | LENS | DISTS

  exception Break

  let huffman kind table off codes =
    let bl_count = Array.make 16 0 in
    let max = ref 15 in

    for sym = 0 to codes - 1 do
      let p = table.(off + sym) in
      bl_count.(p) <- bl_count.(p) + 1
    done ;

    (* XXX(dinosaure): check if we have an incomplete set for [LENS] and [DIST].
       This code is ugly, TODO! *)
    ( try while !max >= 1 do
          if bl_count.(!max) != 0 then raise Break
        ; decr max done with Break -> () ) ;

    let code = ref 0 in
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
    done ; (prefix !ordered !max, !max) (* allocation *)

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
    | `Channel _ -> assert false (* TODO *)
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

  let extra_lbits =
    [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5
     ; 5; 5; 5; 0 |]

  let n_extra_lbits = Array.map Nativeint.of_int extra_lbits

  let extra_dbits =
    [| 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10
     ; 11; 11; 12; 12; 13; 13; 0; 0 |]

  let n_extra_dbits = Array.map Nativeint.of_int extra_dbits

  let base_length =
    [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 10; 12; 14; 16; 20; 24; 28; 32; 40; 48; 56; 64
     ; 80; 96; 112; 128; 160; 192; 224; 255 |]

  let base_dist =
    [| 0; 1; 2; 3; 4; 6; 8; 12; 16; 24; 32; 48; 64; 96; 128; 192; 256; 384; 512
     ; 768; 1024; 1536; 2048; 3072; 4096; 6144; 8192; 12288; 16384; 24576; (-1); (-1) |]

  (* XXX(dinosaure): [zlib] raises "Invalid distance code" where it wants to
     access to [base_dist.(30|31)]. It uses a smart mask to catch this behavior.
     In this code, we did not raise an error nor /compromise/ output when we
     fall to [Match (len:?, dist:0)] (so, nothing to do).

     Case can be retrieved with "\x02\x7e\xff\xff". NOTE: [miniz] has this
     behavior. *)

  let rec c_put_byte byte k d =
    if d.o_pos < bigstring_length d.o
    then ( unsafe_set_uint8 d.o d.o_pos byte
         ; Window.add d.w byte
         ; d.o_pos <- d.o_pos + 1
         ; k d )
    else ( d.k <- c_put_byte byte k (* allocation *)
         ; Flush )

  let slow_inflate lit dist jump d =
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
      c_peek_bits lit.Lookup.l k d
    | Extra_length ->
      let len = extra_lbits.(d.l) in
      let k d =
        let extra = d.hold land ((1 lsl len) - 1) in
        d.hold <- d.hold lsr len ;
        d.bits <- d.bits - len ;
        d.l <- base_length.(d.l) + 3 + extra ;
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
      let len = extra_dbits.(d.d) in
      let k d =
        let extra = d.hold land ((1 lsl len) - 1) in
        d.hold <- d.hold lsr len ;
        d.bits <- d.bits - len ;
        d.d <- base_dist.(d.d) + 1 + extra ;
        d.jump <- Write ;
        d.s <- Inflate ; (* allocation *)
        K in
      c_peek_bits len k d
    | Write ->
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

  exception End
  exception Invalid_distance

  let inflate lit dist jump d =
    let hold = ref (Nativeint.of_int d.hold) in
    let bits = ref d.bits in
    let jump = ref jump in
    let i_pos = ref d.i_pos in
    let o_pos = ref d.o_pos in

    let lit_mask = Nativeint.of_int lit.Lookup.m in
    let dist_mask = Nativeint.of_int dist.Lookup.m in

    (* XXX(dinosaure): 2 jumps were done in this hot-loop (1- [while], 2- [match .. with]). a [let rec length =
       .. and extra_length = ..] can be optimized by [flambda]. We should
       replace [match .. with] by this design. TODO. *)

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
          else if value == 256 then raise End
          else ( jump := Extra_length
               ; d.l <- value - 257 )
        | Extra_length ->
          let len = extra_lbits.(d.l) in
          if !bits < len
          then ( hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
               ; bits := !bits + 8
               ; incr i_pos ) ;
          let extra = Nativeint.(to_int (logand !hold (sub (shift_left 1n len) 1n))) in

          hold := Nativeint.shift_right_logical !hold len ;
          bits := !bits - len ;
          d.l <- base_length.(d.l) + 3 + extra ;
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
          let len = extra_dbits.(d.d) in
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
          d.d <- base_dist.(d.d) + 1 + extra ;

          (* if d.d > Window.have d.w then raise Invalid_distance ;
             XXX(dinosaure): [Window.have] does not tell me the truth where
             we need a read cursor in [Window.t] for that. *)

          jump := Write
        | Write ->
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
      if t.(256) == 0 then raise Invalid_huffman ;
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
    let ret r d = make_table r hlit hdist d in
    (* XXX(dinosaure): [prv] and [i] are stored as associated env of [go]. We
       can not retake them from [d.s]. *)
    let rec go prv i v d =
      match v with
      | 16 ->
        let k n d =
          if i + n + 3 > max_res then err_invalid_dictionary d
          else
            ( for j = 0 to n + 3 - 1 do res.(i + j) <- prv done
            ; if i + n + 3 < max_res then get (go prv (i + n + 3)) d else ret res d ) in
        get_bits 2 k d
      | 17 ->
        let k n d =
          if i + n + 3 > max_res then err_invalid_dictionary d
          else
            (if i + n + 3 < max_res then get (go prv (i + n + 3)) d else ret res d) in
        get_bits 3 k d
      | 18 ->
        let k n d =
          if i + n + 11 > max_res then err_invalid_dictionary d
          else
            (if i + n + 11 < max_res then get (go prv (i + n + 11)) d else ret res d) in
        get_bits 7 k d
      | n ->
        if n < 16
        then
          ( res.(i) <- n
          ; if i + 1 < max_res
            (* XXX(dinosaure): [prv] is the last [n < 16] or the last [v]? *)
            then get (go n (i + 1)) d
            else ret res d )
        else err_invalid_dictionary d in
    let k v d = go 0 0 v d in
    get k d

  let zigzag = [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]

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
          match (d.hold land 0x6) lsr 1 with
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
      else slow_inflate d.literal d.distance d.jump d
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
      | `Channel _ -> assert false in
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
    type t = { buffer : int array
             ; mutable length : int }

    let make size = { buffer= Array.make (size * 2) 0; length= 0 }
    let get_parent i = (i - 2) / 4 * 2
    let get_child i = (2 * i) + 2

    exception Break

    let push index value ({ buffer; length; } as heap) =
      let swap i j =
        let t = buffer.(i) in
        buffer.(i) <- buffer.(j) ;
        buffer.(j) <- t
      in
      buffer.(length) <- value ;
      buffer.(length + 1) <- index ;
      let current = ref length in
      ( try
          while !current > 0 do
            let parent = get_parent !current in
            if buffer.(!current) > buffer.(parent) then (
              swap !current parent ;
              swap (!current + 1) (parent + 1) ;
              current := parent )
            else raise Break
          done
        with Break -> () ) ;
      heap.length <- length + 2

    let pop ({ buffer; length; } as heap) =
      let[@inline] swap i j =
        let t = buffer.(i) in
        buffer.(i) <- buffer.(j) ;
        buffer.(j) <- t
      in
      let value = buffer.(0) in
      let index = buffer.(1) in
      heap.length <- length - 2 ;
      buffer.(0) <- buffer.(heap.length) ;
      buffer.(1) <- buffer.(heap.length + 1) ;
      let parent = ref 0 in
      ( try
          while true do
            let current = get_child !parent in
            if current >= heap.length then raise Break ;
            let current =
              if
                current + 2 < heap.length
                && buffer.(current + 2) > buffer.(current)
              then current + 2
              else current
            in
            if buffer.(current) > buffer.(!parent) then (
              swap current !parent ;
              swap (current + 1) (!parent + 1) )
            else raise Break ;
            parent := current
          done
        with Break -> () ) ;
      (index, value)

    let length {length; _} = length
  end

  let reverse_package_merge p n limit =
    let minimum_cost = Array.make limit 0 in
    let flag = Array.make limit 0 in
    let code_length = Array.make n limit in
    let current_position = Array.make limit 0 in
    let excess = ref ((1 lsl limit) - n) in
    let half = 1 lsl (limit - 1) in
    minimum_cost.(limit - 1) <- n ;
    for j = 0 to limit - 1 do
      if !excess < half then flag.(j) <- 0
      else (
        flag.(j) <- 1 ;
        excess := !excess - half ) ;
      excess := !excess lsl 1 ;
      if limit - 2 - j >= 0 then
        minimum_cost.(limit - 2 - j) <- (minimum_cost.(limit - 1 - j) / 2) + n
    done ;
    minimum_cost.(0) <- flag.(0) ;
    let value =
      Array.init limit (function
        | 0 -> Array.make minimum_cost.(0) 0
        | j ->
            if minimum_cost.(j) > (2 * minimum_cost.(j - 1)) + flag.(j) then
              minimum_cost.(j) <- (2 * minimum_cost.(j - 1)) + flag.(j) ;
            Array.make minimum_cost.(j) 0 )
    in
    let ty = Array.init limit (fun j -> Array.make minimum_cost.(j) 0) in
    (* Decrease codeword lengths indicated by the first element in [ty.(j)],
      recursively accessing other lists if that first element is a package. *)
    let rec take_package j =
      let x = ty.(j).(current_position.(j)) in
      if x = n then (
        take_package (j + 1) ;
        take_package (j + 1) )
      else code_length.(x) <- code_length.(x) - 1 ;
      (* remove and discard the first elements of queues [value.(j)] and
        [ty.(j)]. *)
      current_position.(j) <- current_position.(j) + 1
    in
    for t = 0 to minimum_cost.(limit - 1) - 1 do
      value.(limit - 1).(t) <- p.(t) ;
      ty.(limit - 1).(t) <- t
    done ;
    if flag.(limit - 1) = 1 then (
      code_length.(0) <- code_length.(0) - 1 ;
      current_position.(limit - 1) <- current_position.(limit - 1) + 1 ) ;
    for j = limit - 2 downto 0 do
      let i = ref 0 in
      let next = ref current_position.(j + 1) in
      for t = 0 to minimum_cost.(j) - 1 do
        let weight =
          if !next + 1 < minimum_cost.(j + 1) then
            value.(j + 1).(!next) + value.(j + 1).(!next + 1)
          else p.(!i)
        in
        if weight > p.(!i) then (
          value.(j).(t) <- weight ;
          ty.(j).(t) <- n ;
          next := !next + 2 )
        else (
          value.(j).(t) <- p.(!i) ;
          ty.(j).(t) <- !i ;
          incr i )
      done ;
      current_position.(j) <- 0 ;
      if flag.(j) = 1 then take_package j
    done ;
    code_length

  exception OK

  let get_lengths freqs limit =
    let length = Array.make (Array.length freqs) 0 in
    ( let heap = Heap.make (2 * 286)
    (* XXX(dinosaure): [286] is [255 + 31 (5 bits)]. We did not count [3] here!
       From zlib point-of-view, [286] is the max of any [freqs] and
       [get_lengths] on [distance] frequencies fits. *) in
      let max_code = ref (-1) in
      (* Construct the initial heap, with the least frequent element in
         heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1].
         heap[0] is not used. See implementation in Heap module. *)
      Array.iteri
        (fun i freq ->
          if freq > 0 then (
            max_code := i ;
            Heap.push i freq heap ) )
        freqs
    ; try
        (* The pkzip format requires that at least one distance code exists, and
           that at least one bit should be sent even if there is only one possible
           code. So to avoid special checks later on we force at least two codes
           of non zero frequency. *)
        while Heap.length heap / 2 < 2 do
          Heap.push (if !max_code < 2 then !max_code + 1 else 0) 1 heap ;
          if !max_code < 2 then incr max_code
        done ;
        let nodes = Array.make (Heap.length heap / 2) (0, 0) in
        let values = Array.make (Heap.length heap / 2) 0 in

        if Array.length nodes = 1
        then ( let index, _ = Heap.pop heap in
               length.(index) <- 1 ; raise OK ) ;
        (* The elements heap[length / 2 + 1 .. length] are leaves of the tree,
           establish sub-heaps of increasing lengths: *)
        for i = 0 to (Heap.length heap / 2) - 1 do
          nodes.(i) <- Heap.pop heap ;
          values.(i) <- nodes.(i) |> snd
        done ;
        (* We can now generate the bit lengths. *)

        let code_length = reverse_package_merge values (Array.length values) limit in
        Array.iteri (fun i (j, _) -> length.(j) <- code_length.(i)) nodes
      with OK -> () )
    ; length

  let _max_supported_huffman_codesize = 32

  let get_codes_from_lengths code_size_limit lengths =
    let count = Array.make (_max_supported_huffman_codesize + 1) 0 in
    let start_code = Array.make (_max_supported_huffman_codesize + 1) 0 in
    let codes = Array.make (Array.length lengths) 0 in
    Array.iter (fun length -> count.(length) <- count.(length) + 1) lengths ;
    let code = ref 0 in
    for i = 1 to code_size_limit do
      start_code.(i) <- !code ;
      code := !code + count.(i) ;
      code := !code lsl 1
    done ;
    for i = 0 to Array.length lengths - 1 do
      code := start_code.(lengths.(i)) ;
      start_code.(lengths.(i)) <- start_code.(lengths.(i)) + 1 ;
      for _ = 0 to lengths.(i) - 1 do
        codes.(i) <- (codes.(i) lsl 1) lor (!code land 1) ;
        code := !code lsr 1
      done
    done ; codes
end

module B = struct
  type cmd = int
  type t = (cmd, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  (* TODO: [nativeint] to avoid tag? *)

  external literal : int -> cmd = "%identity"

  let copy ~off ~len : cmd =
    assert (len >= 3 && len <= 258) ;
    assert (off <= 32768) ;
    (len lsl 16) lor off lor 0x2000000 [@@inline]
  (* XXX(dinosaure): note about this representation. If we use a naive repr. like (int * int), we will have a memory problem.
     [len] can not be upper than [255 + 3 + 31 (5 bits)] according RFC 1951.
     [dist] can not be upper than [24576 + 1 + 8191 (13 bits)] according RFC 1951.

     That means, we need at least 9 bits to store [len] and 16 bits to store [dist] -> 25 bits to store a [Copy] op-code. *)

  let length x = Bigarray.Array1.dim x [@@inline]
  external unsafe_get : t -> int -> int = "%caml_ba_ref_1"
  external unsafe_set : t -> int -> int -> unit = "%caml_ba_set_1"

  let of_list l =
    let len = List.length l in
    let res = Bigarray.Array1.create Bigarray.int Bigarray.c_layout len in
    List.iteri
      (fun i -> function
         | `Literal code -> unsafe_set res i code
         | `Copy (off, len) -> unsafe_set res i (copy ~off ~len lor 0x2000000))
      l ;
    res

  let to_list t =
    let res = ref [] in

    for i = length t - 1 downto 0
    do let cmd = unsafe_get t i in
      if cmd land 0x2000000 != 0
      then res := `Copy (cmd lsr 16, cmd land 0xffff) :: !res
      else res := `Literal cmd :: !res
    done ; !res
end

module N = struct
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type code = int

  type dynamic =
    { ltree : Lookup.t
    ; dtree : Lookup.t
    ; h_lit : int
    ; h_dst : int
    ; h_len : int
    ; zigzag : int array
    ; deflated : int array * int array * int array }

  let deflate_trees h_lit lit_lengths h_dst dst_lengths =
    let len = h_lit + h_dst in
    let src = Array.make len 0 in
    let res = Array.make (286 + 30) 0 in
    let freqs = Array.make 19 0 in

    for i = 0 to h_lit - 1 do src.(i) <- lit_lengths.(i) done ;
    for i = h_lit to h_lit + h_dst - 1 do src.(i) <- dst_lengths.(i - h_lit) done ;

    let n = ref 0 in
    let i = ref 0 in

    while !i < len do
      let j = ref 1 in

      while !i + !j < len && src.(!i + !j) == src.(!i)
      do incr j done ;

      let run_length = ref !j in

      if src.(!i) == 0 then
        if !run_length < 3 then
          while !run_length > 0 do
            res.(!n) <- 0 ;
            freqs.(0) <- freqs.(0) + 1 ;
            incr n ;
            decr run_length ;
          done
        else
          while !run_length > 0 do
            let rpt = ref (if !run_length < 138 then !run_length else 138) in
            (* XXX(dinosaure): replace by [min]? *)

            if !rpt > !run_length - 3 && !rpt < !run_length
            then rpt := !run_length - 3 ;
            if !rpt <= 10 then
              ( res.(!n) <- 17
              ; incr n
              ; res.(!n) <- !rpt - 3
              ; incr n
              ; freqs.(17) <- freqs.(17) + 1 )
            else
              ( res.(!n) <- 18
              ; incr n
              ; res.(!n) <- (!rpt - 11)
              ; incr n
              ; freqs.(18) <- freqs.(18) + 1 ) ;
            run_length := !run_length - !rpt
          done
      else
        ( res.(!n) <- src.(!i)
        ; incr n
        ; freqs.(src.(!i)) <- freqs.(src.(!i)) + 1
        ; decr run_length
        ; if !run_length < 3 then
            while !run_length > 0 do
              res.(!n) <- src.(!i) ;
              incr n ;
              freqs.(src.(!i)) <- freqs.(src.(!i)) + 1 ;
              decr run_length ;
            done
          else
            while !run_length > 0 do
              let rpt = ref (if !run_length < 6 then !run_length else 6) in
              (* XXX(dinosaure): [min]? *)
              if !rpt > !run_length - 3 && !rpt < !run_length
              then rpt := !run_length - 3 ;

              res.(!n) <- 16 ;
              incr n ;
              res.(!n) <- (!rpt - 3) ;
              incr n ;
              freqs.(16) <- freqs.(16) + 1 ;
              run_length := !run_length - !rpt
            done ) ;
      i := !i + !j
    done ;
    Array.sub res 0 !n, freqs

  (* XXX(dinosaure): this part needs a big check. *)

  let dynamic_of_frequencies
    : literals:int array -> distances:int array -> dynamic
    = fun ~literals:lit_freqs ~distances:dst_freqs ->
    let lit_lengths = T.get_lengths lit_freqs 15 in
    let dst_lengths = T.get_lengths dst_freqs 15 in
    let lit_codes = T.get_codes_from_lengths 15 lit_lengths in
    let dst_codes = T.get_codes_from_lengths 15 dst_lengths in

    let h_lit = ref 286 in while !h_lit >= 256 && lit_lengths.(!h_lit - 1) == 0 do decr h_lit done ;
    let h_dst = ref 30 in while !h_dst >= 1 && dst_lengths.(!h_dst - 1) == 0 do decr h_dst done ;

    let zigzag = Array.make 19 0 in

    let def_symbols, freqs = deflate_trees !h_lit lit_lengths !h_dst dst_lengths in
    let def_lengths = T.get_lengths freqs 7 in
    let def_codes = T.get_codes_from_lengths 7 def_lengths in

    for i = 0 to 18 do zigzag.(i) <- def_lengths.(M.zigzag.(i)) done ;
    let h_len = ref 19 in while !h_len > 4 && zigzag.(!h_len - 1) = 0 do decr h_len done ;

    let ltree, ltree_max =
      let max = ref 0 in
      let res = Array.map2 (fun code len -> if len > !max then max := len ; (len lsl 15) lor code) lit_codes lit_lengths in
      res, !max in
    let dtree, dtree_max =
      let max = ref 0 in
      let res = Array.map2 (fun code len -> if len > !max then max := len ; (len lsl 15) lor code) dst_codes dst_lengths in
      res, !max in

    { deflated= def_symbols, def_codes, def_lengths
    ; h_lit= !h_lit
    ; h_dst= !h_dst
    ; h_len= !h_len
    ; zigzag
    ; ltree= Lookup.make ltree ltree_max
    ; dtree= Lookup.make dtree dtree_max }

  let invalid_encode () = Fmt.invalid_arg "expected `Await encode"

  let unsafe_blit src src_off dst dst_off len =
    let a = bigstring_sub src src_off len in
    let b = bigstring_sub dst dst_off len in
    bigstring_blit a b

  type encode = [ `Literal of code | `Copy of offset * length | `Await | `End ]
  and offset = int and length = int

  type block = Flat of int | Fixed | Dynamic of dynamic

  type encoder =
    { dst : dst
    ; blk : block
    ; mutable hold : int
    ; mutable bits : int
    ; mutable o : bigstring
    ; mutable o_pos : int
    ; mutable o_max : int
    ; mutable f_pos : int
    ; t : bigstring
    ; mutable t_pos : int
    ; mutable t_max : int
    ; mutable k : encoder -> encode -> [ `Ok | `Partial ] }

  (* remaining bytes to write in [e.o]. *)
  let o_rem e = e.o_max - e.o_pos + 1
  let t_rem e = e.t_max - e.t_pos + 1 [@@inline]

  (* set [e.o] with [s]. *)
  let dst e s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s) then invalid_bounds j l ;
    e.o <- s ;
    e.o_pos <- j ;
    e.o_max <- j + l - 1

  let partial k e = function
    | `Await -> k e
    (* if [encode] returns [`Partial], end-user must call [encode] with [`Await]. *)
    | `Literal _ | `Copy _ | `End -> invalid_encode ()

  let flush k e = match e.dst with
    | `Manual -> e.k <- partial k ; `Partial
    | `Channel oc ->
      output_bigstring oc e.o 0 e.o_pos ; e.o_pos <- 0 ; k e
    | `Buffer b ->
      for i = 0 to e.o_pos - 1
      do Buffer.add_char b (Char.unsafe_chr (unsafe_get_uint8 e.o i)) done ;
      (* TODO: check why we need [unsafe_chr]. *)
      e.o_pos <- 0 ; k e

  let t_range e max = e.t_pos <- 0 ; e.t_max <- max

  let rec t_flush k e =
    let blit e l =
      unsafe_blit e.t e.t_pos e.o e.o_pos l ;
      e.o_pos <- e.o_pos + l ; e.t_pos <- e.t_pos + l in
    let rem = o_rem e in
    let len = e.t_max - e.t_pos + 1 in

    if rem < len
    then ( blit e rem ; flush (t_flush k) e )
    else ( blit e len ; k e )

  let _flush_hold_len = Sys.word_size - 1 - 20
  let _flush_hold_dst = Sys.word_size - 1 - 28

  let w_encode k e =
    let rem = o_rem e in
    let len = e.bits / 8 in (* to bytes *)
    let s, j, k =
      if rem < len then ( t_range e (len - 1) ; e.t, 0, t_flush k )
      else let j = e.o_pos in ( e.o_pos <- e.o_pos + len ; e.o, j, k ) in
    for i = 0 to len - 1
    do unsafe_set_uint8 s (j + i) (e.hold land 0xff)
     ; e.hold <- e.hold lsr 8
    done ;
    e.bits <- e.bits - (len * 8) ;
    k e

  let w_flat _ = assert false

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
       ; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 0; 0; 16; 17
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
       ; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29
      |]
    in
    if code < 256 then t.(code) else t.(256 + (code lsr 7)) [@@inline]

  let _base_length =
    [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 10; 12; 14; 16; 20; 24; 28; 32; 40; 48; 56; 64
     ; 80; 96; 112; 128; 160; 192; 224; 255 |]

  let _base_dist =
    [| 0; 1; 2; 3; 4; 6; 8; 12; 16; 24; 32; 48; 64; 96; 128; 192; 256; 384; 512
     ; 768; 1024; 1536; 2048; 3072; 4096; 6144; 8192; 12288; 16384; 24576 |]

  let _extra_lbits =
    [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5
     ; 5; 5; 5; 0 |]

  let _extra_dbits =
    [| 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10
     ; 11; 11; 12; 12; 13; 13 |]

  let rec encode e v =
    let k e = e.k <- encode ; `Ok in
    match v, e.blk with
    | `Await, _ -> k e
    | `End, _ -> flush k e
    | `Literal code, Dynamic dynamic ->
      let v, len = Lookup.get dynamic.ltree code in

      let k e =
        e.hold <- (v lsl e.bits) lor e.hold ;
        e.bits <- e.bits + len ;
        k e in

      if e.bits > _flush_hold_len then w_encode k e else k e
    | `Copy (off, len), Dynamic dynamic ->
      let v0, len0 = Lookup.get dynamic.ltree (_length.(len) + 256 + 1) in
      let v1, len1 =
        let code = _length.(len) in
        len - _base_length.(code), _extra_lbits.(code) in
      let v2, len2 = Lookup.get dynamic.dtree (_distance off) in
      let v3, len3 =
        let code = _distance off in
        off - _base_dist.(code), _extra_dbits.(code) in

      (* XXX(dinosaure): a really slow writer.
         [len0 + len1 + len2 + len3 <= 48] so we have 2 problems:
         - [e.hold] is full, so we need to flush it (see [w_encode]).
         - it's a 32-bits machine, so we can not store [length] ([v0] and [v1])
           and [distance] ([v2] and [v3]) atomically.

         So we need:
         - to check if we have enough space to store [length] and, then, [distance] in
           [e.hold] (see [_flush_hold_len] and [_flush_hold_len])
         - to flush 2 times:
          * one for [length] (max 20 bits)
          * second for [distance] (max 28 bits)
      *)

      let k1 e =
        e.hold <- (v3 lsl (e.bits + len2)) lor (v2 lsl e.bits) lor e.hold ;
        e.bits <- e.bits + len2 + len3 ;
        k e in

      let k0 e =
        e.hold <- (v1 lsl (e.bits + len0)) lor (v0 lsl e.bits) lor e.hold ;
        e.bits <- e.bits + len0 + len1 ;
        if e.bits > _flush_hold_dst then w_encode k1 e else k1 e in

      if e.bits > _flush_hold_len then w_encode k0 e else k0 e
    | `Literal code, Flat max ->
      if e.f_pos == max then w_flat e
      else
        let rem = o_rem e in
        if rem < 1 then flush (fun e -> encode e v) e
        else
          ( unsafe_set_uint8 e.o e.o_pos code
          ; e.o_pos <- e.o_pos + 1
          ; e.f_pos <- e.f_pos + 1
          ; k e )
    | `Copy _, Flat _ -> Fmt.invalid_arg "Impossible to store a copy code inner a flat block"
    | _, _ -> assert false
end
