type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_length x = Bigarray.Array1.dim x [@@inline]
let bigstring_create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"

let invalid_bounds off len = Fmt.invalid_arg "Out of bounds (off: %d, len: %d)" off len

let blit2 src src_off dst0 dst0_off dst1 dst1_off len =
  for i = 0 to len - 1 do
    unsafe_set_uint8 dst0 (dst0_off + i) (unsafe_get_uint8 src (src_off + i)) ;
    unsafe_set_uint8 dst1 (dst1_off + i) (unsafe_get_uint8 src (src_off + i))
  done

let io_buffer_size = 65536

external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let ( > ) (x : int) y = x > y [@@inline]
let ( < ) (x : int) y = x < y [@@inline]
let ( <= ) (x : int) y = x <= y [@@inline]

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
    let v = Array.unsafe_get t.t i in v lsr 15, v land mask (* allocation *)
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

  let huffman table off codes =
    let bl_count = Array.make 16 0 in
    for sym = 0 to codes - 1 do
      let p = table.(off + sym) in
      bl_count.(p) <- bl_count.(p) + 1
    done ;
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
    if !left > 0 then raise Invalid_huffman ;
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

  let i_rem d = d.i_len - d.i_pos + 1
  [@@inline]

  let src d s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    if (l == 0) then eoi d
    else
      ( d.i <- s
      ; d.i_pos <- j
      ; d.i_len <- j + l - 1 )

  let refill k d = match d.src with
    | `String _ ->
      eoi d ; k d
    | `Channel _ -> assert false
    | `Manual ->
      d.k <- k ; Await

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
    let tbl_lit, max_lit = huffman tbl_lit 0 288 in
    Lookup.make tbl_lit max_lit, Lookup.make tbl_dist 5

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
           then ( d.s <- Checkseum ; checksum d )
           else ( d.s <- Header ; K ) )
    else match i_rem d, bigstring_length d.o - d.o_pos with
      | 0, _ -> Await
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

  let extra_dbits =
    [| 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10
     ; 11; 11; 12; 12; 13; 13 |]

  let base_length =
    [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 10; 12; 14; 16; 20; 24; 28; 32; 40; 48; 56; 64
     ; 80; 96; 112; 128; 160; 192; 224; 255 |]

  let base_dist =
    [| 0; 1; 2; 3; 4; 6; 8; 12; 16; 24; 32; 48; 64; 96; 128; 192; 256; 384; 512
     ; 768; 1024; 1536; 2048; 3072; 4096; 6144; 8192; 12288; 16384; 24576 |]

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
        let len, value = Lookup.get lit (d.hold land lit.Lookup.m) in
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
               then ( d.s <- Checkseum ; checksum d )
               else ( d.s <- Header ; K ) )
        else ( d.l <- value - 257
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
        d.s <- Inflate ; (* allocation *)
        K in
      c_peek_bits len k d
    | Distance ->
      let k d =
        let len, value = Lookup.get dist (d.hold land dist.Lookup.m) in
        d.hold <- d.hold lsr len ;
        d.bits <- d.bits - len ;
        d.d <- value ;
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
      then ( d.s <- Inflate (* allocation *)
           ; K )
      else ( d.l <- d.l - len
           ; d.s <- Inflate (* allocation *)
           ; Flush )

  exception End

  let inflate lit dist jump d =
    let hold = ref d.hold in
    let bits = ref d.bits in
    let jump = ref jump in
    let i_pos = ref d.i_pos in
    let o_pos = ref d.o_pos in

    try while d.i_len - !i_pos + 1 > 1
          && !o_pos < bigstring_length d.o
      do match !jump with
        | Length ->
          if !bits < lit.Lookup.l
          then ( hold := !hold lor (unsafe_get_uint8 d.i !i_pos lsl !bits)
               ; bits := !bits + 8
               ; incr i_pos
               ; hold := !hold lor (unsafe_get_uint8 d.i !i_pos lsl !bits)
               ; bits := !bits + 8
               ; incr i_pos ) ;
          let len, value = Lookup.get lit (!hold land lit.Lookup.m) in
          hold := !hold lsr len ;
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
          then ( hold := !hold lor (unsafe_get_uint8 d.i !i_pos lsl !bits)
               ; bits := !bits + 8
               ; incr i_pos ) ;
          let extra = !hold land ((1 lsl len) - 1) in
          hold := !hold lsr len ;
          bits := !bits - len ;
          d.l <- base_length.(d.l) + 3 + extra ;
          jump := Distance
        | Distance ->
          if !bits < dist.Lookup.l
          then ( hold := !hold lor (unsafe_get_uint8 d.i !i_pos lsl !bits)
              ; bits := !bits + 8
              ; incr i_pos
              ; hold := !hold lor (unsafe_get_uint8 d.i !i_pos lsl !bits)
              ; bits := !bits + 8
              ; incr i_pos ) ;
          let len, value = Lookup.get dist (!hold land dist.Lookup.m) in
          hold := !hold lsr len ;
          bits := !bits - len ;
          d.d <- value ;
          jump := Extra_distance
        | Extra_distance ->
          let len = extra_dbits.(d.d) in
          if !bits < len
          then ( hold := !hold lor (unsafe_get_uint8 d.i !i_pos lsl !bits)
               ; bits := !bits + 8
               ; incr i_pos
               ; hold := !hold lor (unsafe_get_uint8 d.i !i_pos lsl !bits)
               ; bits := !bits + 8
               ; incr i_pos ) ;
          let extra = !hold land ((1 lsl len) - 1) in
          hold := !hold lsr len ;
          bits := !bits - len ;
          d.d <- base_dist.(d.d) + 1 + extra ;
          jump := Write
        | Write ->
          let len = min d.l (bigstring_length d.o - !o_pos) in
          let off = Window.mask (d.w.Window.w - d.d) in
          let pre = Window.max - off in
          let rst = len - pre in
          if rst > 0
          then ( Window.blit d.w d.w.Window.raw off d.o !o_pos pre
               ; Window.blit d.w d.w.Window.raw 0 d.o (!o_pos + pre) rst )
          else Window.blit d.w d.w.Window.raw off d.o !o_pos len ;
          o_pos := !o_pos + len ;
          if d.l - len == 0 then jump := Length else d.l <- d.l - len
      done ;

      d.hold <- !hold ;
      d.bits <- !bits ;
      d.i_pos <- !i_pos ;
      d.o_pos <- !o_pos ;
      d.jump <- !jump ;
      d.k <- slow_inflate lit dist !jump ; (* allocation *)
      d.s <- Slow ;

      if i_rem d > 0 then Flush else Await
    with End ->
      d.hold <- !hold ;
      d.bits <- !bits ;
      d.i_pos <- !i_pos ;
      d.o_pos <- !o_pos ;

      if d.last
      then ( d.s <- Checkseum
           ; checksum d )
      else ( d.s <- Header
           ; K )

  let fixed d =
    let lit, dist = fixed_lit, fixed_dist in
    d.literal <- lit ;
    d.distance <- dist ;
    d.jump <- Length ;
    d.s <- Inflate ; (* allocation *)
    inflate lit dist Length d

  let make_table t hlit hdist d =
    let t_lit, l_lit = huffman t 0 hlit in
    let t_dist, l_dist = huffman t hlit hdist in

    let lit = Lookup.make t_lit l_lit in
    let dist = Lookup.make t_dist l_dist in

    d.literal <- lit ;
    d.distance <- dist ;
    d.jump <- Length ;
    d.s <- Inflate ; (* allocation *)
    inflate lit dist Length d

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

    let t, l = huffman res 0 19 in

    d.hold <- !hold ;
    d.bits <- !bits ;
    d.s <- Inflate_table { t; l
                         ; r= Array.make (hlit + hdist) 0
                         ; h= (hlit, hdist, hclen) } ;
    inflate_table d

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
      let l_header d =
        assert (d.bits >= 3) ; (* allocation *)
        let last = d.hold land 1 == 1 in
        let k = match (d.hold land 0x6) lsr 1 with
          | 0 -> flat_header
          | 1 -> fixed
          | 2 -> dynamic
          | 3 -> err_invalid_kind_of_block
          | _ -> assert false in
        d.last <- last ;
        d.hold <- d.hold lsr 3 ;
        d.bits <- d.bits - 3 ;
        k d in
      c_peek_bits 3 l_header d
    | Table { hclen; _ } -> c_peek_bits (hclen * 3) table d
    | Inflate_table _ -> d.k d
    | Inflate ->
      if i_rem d > 1
      then inflate d.literal d.distance d.jump d
      else slow_inflate d.literal d.distance d.jump d
    | Slow -> d.k d
    | Flat -> flat d
    | Checkseum -> checksum d
    | End_of_inflate -> d.k d

  let rec decode d = match decode_k d with
    | Await -> `Await
    | Flush -> `Flush
    | End -> `End
    | Malformed err -> `Malformed err
    | K -> decode d

  let dst_rem d = bigstring_length d.o - d.o_pos
  let flush d = d.o_pos <- 0

  let decoder src o w =
    let i, i_pos, i_len = match src with
      | `Manual -> bigstring_empty, 1, 0
      | `Channel _ | `String _ -> assert false in
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
