let unsafe_get_uint8 _ _ = assert false
let unsafe_set_uint8 _ _ _ = ()
let invalid_bounds _ _ = assert false
let blit _ _ _ _ _ = ()
let blit2 _ _ _ _ _ _ _ = ()

let io_buffer_size = 65536

let (//) x y =
  if y < 1 then raise Division_by_zero ;
  if x > 0 then 1 + ((x - 1) / y) else 0
[@@inline]

let min (a : int) b = min a b

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
end

module Lookup = struct
  type t =
    { t : int array
    ; m : int
    ; l : int }

  let make t m = { t; m= (1 lsl m) - 1; l= m }

  let get t i =
    let v = t.t.(i) in v lsr 15, v land t.m
end

module Window = struct
  type t =
    { raw : Bytes.t
    ; mutable r : int
    ; mutable w : int
    ; mutable c : int32 }

  let max = 1 lsl 15
  let mask = (1 lsl 15) - 1
  let ( = ) (a : int) b = a = b

  let make () =
    { raw= Bytes.create max
    ; r= 0
    ; w= 0
    ; c= 0l }

  let mask v = v land mask
  [@@inline]
  let empty t = t.r = t.w
  [@@inline]
  let size t = t.w - t.r
  [@@inline]
  let available t = max - (t.w - t.r)
  [@@inline]
  let full t = size t = max
  [@@inline]

  let update _ = ()

  exception Full

  let add t v =
    if (full [@inlined]) t then raise Full ;
    if (mask [@inlined]) (t.w + 1) == 0 then update t ;
    unsafe_set_uint8 t.raw ((mask [@inlined]) t.w) v ;
    t.w <- t.w + 1

  let blit t w w_off o o_off len =
    let pre = max - t.w in
    let rst = len - pre in
    if rst > 0
    then ( blit2 w w_off t.raw t.w o o_off pre
         ; update t
         ; blit2 w (w_off + pre) t.raw 0 o (o_off + pre) rst )
    else blit2 w w_off t.raw t.w o o_off len ;
    t.w <- t.w + len

  let checkseum _ = 0l
end

module M = struct
  (* à la dbuenzli *)

  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Await | `Flush of Bytes.t | `End | `Continuation | `Malformed of string ]

  let malformedf fmt = Fmt.kstrf (fun s -> `Malformed s) fmt

  exception Break
  exception Invalid_huffman

  let prefix heap max =
    assert (max < 16) ;
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
          aux (if incr <> 0 then (huff land (incr - 1)) + incr else 0) heap
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
        ordered := Heap.push !ordered n (l, i) ;
        max := if l > !max then l else !max )
    done ;
    (prefix !ordered !max, !max)

  type decoder =
    { src : src
    ; mutable i : Bytes.t
    ; mutable i_pos : int
    ; mutable i_len : int
    ; mutable hold : int
    ; mutable bits : int
    ; mutable last : bool
    ; o : Bytes.t
    ; mutable o_pos : int
    ; mutable l : int (* literal *)
    ; mutable d : int (* distance *)
    ; w : Window.t
    ; mutable s : state
    ; mutable k : decoder -> decode }
  and state =
    | Header
    | Table of { hlit : int
               ; hdist : int
               ; hclen : int }
    | Inflate_table of { t : int array
                       ; l : int
                       ; r : int array
                       ; h : int * int * int }
    | Inflate of { lit : Lookup.t
                 ; dist : Lookup.t
                 ; jump : jump }
    | Checkseum of int32
  and jump = Length | Extra_length | Distance | Extra_distance | Write

  let eoi d =
    d.i <- Bytes.empty ;
    d.i_pos <- 0 ;
    d.i_len <- min_int

  let err_unexpected_end_of_input d =
    let k _ = `End in
    eoi d ; d.k <- k ;
    malformedf "Unexpected end of input"

  let err_invalid_kind_of_block d =
    let k _ = `End in
    eoi d ; d.k <- k ;
    malformedf "Invalid kind of block"

  let err_invalid_dictionary d =
    let k _ = `End in
    eoi d ; d.k <- k ;
    malformedf "Invalid dictionary"

  let i_rem d = d.i_len - d.i_pos + 1
  [@@inline]

  let src d s j l =
    if (j < 0 || l < 0 || j + l > Bytes.length s)
    then invalid_bounds j l ;
    if (l = 0) then eoi d
    else
      ( d.i <- s
      ; d.i_pos <- j
      ; d.i_len <- j + l - 1 )

  let refill k d = match d.src with
    | `String _ ->
      eoi d ; k d
    | `Channel ic ->
      let l = input ic d.i 0 (Bytes.length d.i) in
      src d d.i 0 l ; k d
    | `Manual ->
      d.k <- k ; `Await

  let rec c_peek_bits n k d =
    if d.bits >= n then k d
    else
      let rem = i_rem d in

      if rem <= 0
      then
        if rem < 0 (* end of input *)
        then err_unexpected_end_of_input d
        else refill (c_peek_bits n k) d
      else
        ( let byte = unsafe_get_uint8 d.i d.i_pos in
          d.i_pos <- d.i_pos + 1
        ; d.hold <- d.hold lor (byte lsl d.bits)
        ; d.bits <- d.bits + 8
        ; k d )

  [@@@warning "-26-27"]

  let flat d = assert false

  let fixed d = assert false

  let checkseum v d = assert false

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

  exception End

  let inflate lit dist jump d =
    let hold = ref d.hold in
    let bits = ref d.bits in
    let jump = ref jump in
    let i_pos = ref d.i_pos in
    let o_pos = ref d.o_pos in

    try while d.i_len - !i_pos + 1 > 1
          && !o_pos < Bytes.length d.o
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
          else if value = 256 then raise End
          else ( jump := Extra_length
              ; d.l <- value )
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
          let len = min d.l (Bytes.length d.o - !o_pos) in
          let off = (Window.mask [@inlined]) (d.w.Window.w - d.d) in
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
      d.s <- Inflate { lit; dist; jump= !jump } ;
      if i_rem d > 0 then `Flush d.o else `Await
    with End ->
      d.hold <- !hold ;
      d.bits <- !bits ;
      d.i_pos <- !i_pos ;
      d.o_pos <- !o_pos ;

      if d.last
      then ( let v = Window.checkseum d.w in
             d.s <- Checkseum v
           ; checkseum v d )
      else ( d.s <- Header
           ; `Continuation )

  let make_table t hlit hdist d =
    let t_lit, l_lit = huffman t 0 15 in
    let t_dist, l_dist = huffman t hlit 15 in

    let lit = Lookup.make t_lit l_lit in
    let dist = Lookup.make t_dist l_dist in
    d.s <- Inflate { lit; dist; jump= Length };
    inflate lit dist Length d

  let inflate_table d =
    let[@warning "-8"] Inflate_table { t; l= max; r; h= (hlit, hdist, _) } = d.s in
    let mask = (1 lsl max) - 1 in
    let get k d =
      let len, v =
        t.(d.hold land mask) lsr 15,
        t.(d.hold land mask) land ((1 lsl 15) - 1) in
      d.hold <- d.hold lsr len ;
      d.bits <- d.bits - len ;
      k v d in
    let get k d = c_peek_bits max (get k) d in
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
    let rec go ?prv i v d =
      match v with
      | 16 ->
        let k n d =
          if i + n + 3 > max then err_invalid_dictionary d
          else match prv with
            | Some prv ->
              for j = 0 to n + 3 - 1 do r.(i + j) <- prv done ;
              if i + n + 3 < max then get (go (i + n + 3)) d else ret r d
            | None -> err_invalid_dictionary d in
        get_bits 2 k d
      | 17 ->
        let k n d =
          if i + n + 3 > max then err_invalid_dictionary d
          else
            (if i + n + 3 < max then get (go (i + n + 3)) d else ret r d) in
        get_bits 3 k d
      | 18 ->
        let k n d =
          if i + n + 11 > max then err_invalid_dictionary d
          else
            (if i + n + 11 < max then get (go (i + n + 11)) d else ret r d) in
        get_bits 7 k d
      | n ->
        if n < 16
        then
          ( r.(i) <- n
          ; if i + 1 < max
            (* XXX(dinosaure): [prv] is the last [n < 16] or the last [v]? *)
            then get (go ~prv:n (i + 1)) d
            else ret r d )
        else err_invalid_dictionary d in
    let k v d = go 0 v d in
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
      assert (d.bits < 7) ; (* XXX(dinosaure): we need, at most, 57 bits. *)
      c_peek_bits (hclen * 3) table d in
    c_peek_bits 14 l_header d

  let decode d = match d.s with
    | Header ->
      let l_header d =
        assert (d.bits >= 3) ;
        let last = d.hold land 1 == 1 in
        let k = match (d.hold land 0x6) lsr 1 with
          | 0 -> flat
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
    | Inflate { lit; dist; jump; } -> inflate lit dist jump d
    | Checkseum v -> checkseum v d

  let dst_rem d = Bytes.length d.o - d.o_pos

  let decoder src len =
    let i, i_pos, i_len = match src with
      | `Manual -> Bytes.empty, 1, 0
      | `Channel _ -> Bytes.create io_buffer_size, 1, 0
      | `String s -> Bytes.unsafe_of_string s, 0, String.length s - 1 in
    let o = Bytes.create len in
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
    ; w= Window.make ()
    ; s= Header
    ; k = decode }
end
