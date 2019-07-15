(** {2 Prelude} *)

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type optint = Optint.t

val bigstring_create : int -> bigstring
val bigstring_length : bigstring -> int
val io_buffer_size : int

val output_bigstring : out_channel -> bigstring -> int -> int -> unit

(** {2 Window} *)

type window

val make_window : bits:int -> window
(** [make_window] allocates a new buffer which represents a {i window}. It used
   by decoder and LZ77 compressor to keep tracking older inputs and:

   {ul
   {- process a copy from a distance by the decoder.}
   {- generate a copy from the compression algorithm.}}*)

val window_bits : window -> int

(** {2 Decoder} *)

module M : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Await | `Flush | `End | `Malformed of string ]

  type decoder

  val decoder : src -> o:bigstring -> w:window -> decoder
  val decode : decoder -> decode

  val src : decoder -> bigstring -> int -> int -> unit
  val dst_rem : decoder -> int
  val src_rem : decoder -> int
  val flush : decoder -> unit
  val checksum : decoder -> optint
end

(** {2 Queue} *)

module B : sig
  type cmd
  type buf
  type t

  exception Full
  exception Empty

  val is_empty : t -> bool
  val is_full : t -> bool
  val length : t -> int
  val available : t -> int

  val push_exn : t -> cmd -> unit
  val pop_exn : t -> cmd
  val junk_exn : t -> int -> unit

  val copy : off:int -> len:int -> cmd
  val literal : char -> cmd
  val eob : cmd
  val cmd : [ `Literal of char | `Copy of int * int | `End ] -> cmd
  val blit : t -> bigstring -> int -> int -> unit

  val create : int -> t
  val reset : t -> unit
  val to_list : t -> [ `Literal of char | `Copy of int * int | `End ] list
  val of_list : [ `Literal of char | `Copy of int * int | `End ] list -> t
end

(** {2 Frequencies} *)

type literals = private int array
type distances = private int array

val make_literals : unit -> literals
val make_distances : unit -> distances

val succ_literal : literals -> char -> unit
val succ_length : literals -> int -> unit
val succ_distance : distances -> int -> unit

(** {2 Encoder} *)

module N : sig
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type dynamic
  type kind = Flat of int | Fixed | Dynamic of dynamic
  type block = { kind: kind; last: bool; }

  type encode = [ `Await | `Flush | `Block of block ]

  val dynamic_of_frequencies : literals:literals -> distances:distances -> dynamic

  type encoder

  val encoder : dst -> q:B.t -> encoder
  val encode : encoder -> encode -> [ `Ok | `Partial | `Block ]

  val dst : encoder -> bigstring -> int -> int -> unit
  val dst_rem : encoder -> int
end

module L : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Flush | `Await | `End ]

  type state

  val literals : state -> literals
  val distances : state -> distances

  val checksum : state -> optint
  val src : state -> bigstring -> int -> int -> unit
  val src_rem : state -> int
  val compress : state -> decode
  val state : src -> w:window -> q:B.t -> state
end

(** {2 Higher API} *)

module Higher : sig
  val compress :
    w:window ->
    q:B.t ->
    i:bigstring ->
    o:bigstring ->
    refill:(bigstring -> int) ->
    flush:(bigstring -> int -> unit) -> unit

  val decompress :
    w:window ->
    i:bigstring ->
    o:bigstring ->
    refill:(bigstring -> int) ->
    flush:(bigstring -> int -> unit) -> unit

  val of_string : o:bigstring -> w:window -> string -> flush:(bigstring -> int -> unit) -> unit
  val to_string : i:bigstring -> w:window -> q:B.t -> refill:(bigstring -> int) -> string
end

(** / **)

module Lookup : sig
  type t = { t : int array; m : int; l : int; }
  val get : t -> int -> int * int
end

module T : sig
  type tree = { lengths : int array; max_code : int; tree : Lookup.t; }
  val make : length:int -> ?max_length:int -> int array -> bl_count:int array -> tree
end
