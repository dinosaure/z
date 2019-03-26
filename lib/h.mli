type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val io_buffer_size : int

module M : sig
  type decode = [ `Await | `Destination of int | `End | `Malformed of string ]

  type decoder

  val src : decoder -> bigstring -> int -> int -> unit
  val dst : decoder -> bigstring -> int -> int -> unit

  val src_rem : decoder -> int
  val dst_rem : decoder -> int

  val decode : decoder -> decode
  val decoder : bigstring -> decoder
end
