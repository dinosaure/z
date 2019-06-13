type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type optint = Optint.t

val bigstring_create : int -> bigstring
val unsafe_get_uint8 : bigstring -> int -> int

val io_buffer_size : int

module Window : sig
  type t

  val max : int
  val make : unit -> t
  val from : bigstring -> t
  val checksum : t -> optint
end

module Lookup : sig
  type t =
    { t : int array
    ; m : int
    ; l : int }

  val mask : int
  val make : int array -> int -> t
  val get : t -> int -> int * int
end

module M : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Await | `Flush | `End | `Malformed of string ]

  type decoder

  val decoder : src -> o:bigstring -> w:bigstring -> decoder
  val decode : decoder -> decode

  val src : decoder -> bigstring -> int -> int -> unit
  val dst_rem : decoder -> int
  val flush : decoder -> unit
end

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

module T : sig
  module Heap : sig
    type t

    val make : unit -> t
    val populate : length:int -> freqs:int array -> int array -> depth:int array -> t -> int
    val pkzip : int -> freqs:int array -> depth:int array -> t -> int
    val pqdownheap : freqs:int array -> depth:int array -> t -> int -> unit
    val pqremove : freqs:int array -> depth:int array -> t -> int
  end

  type tree =
    { lengths : int array
    ; max_code : int
    ; tree : Lookup.t }

  val generate_codes : tree_lengths:int array -> max_code:int -> bl_count:int array -> int array
  val generate_lengths : tree_dads:int array -> tree_lengths:int array -> max_code:int -> max_length:int -> Heap.t -> bl_count:int array -> unit
  val make : length:int -> ?max_length:int -> int array -> bl_count:int array -> tree
  val scan : int array -> int -> bl_freqs:int array -> unit
end

module N : sig
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type dynamic
  type kind = Flat of int | Fixed | Dynamic of dynamic
  type block = { kind: kind; last: bool; }

  type encode = [ `Await | `Flush | `Block of block ]

  type literals
  type distances

  val pp_literals : literals Fmt.t
  val pp_distances : distances Fmt.t
  val unsafe_literals_to_array : literals -> int array
  val unsafe_distances_to_array : distances -> int array

  val make_literals : unit -> literals
  val make_distances : unit -> distances

  val succ_literal : literals -> char -> unit
  val succ_length : literals -> int -> unit
  val succ_distance : distances -> int -> unit

  val dynamic_of_frequencies : literals:literals -> distances:distances -> dynamic

  type encoder

  val encoder : dst -> q:B.t -> encoder
  val encode : encoder -> encode -> [ `Ok | `Partial | `Block ]

  val dst : encoder -> bigstring -> int -> int -> unit
  val dst_rem : encoder -> int
end

module W : sig
  val max : int
end

module L : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Flush | `Await | `End ]

  type state

  val literals : state -> N.literals
  val distances : state -> N.distances

  val src : state -> bigstring -> int -> int -> unit
  val compress : state -> decode
  val state : src -> w:bigstring -> q:B.t -> state
end
