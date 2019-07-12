type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type window = Dd.window 

val io_buffer_size : int

module M : sig
  type decoder

  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Await of decoder | `Flush of decoder | `End of decoder | `Malformed of string ]

  val decoder : src -> o:bigstring -> allocate:(int -> window) -> decoder
  val decode : decoder -> decode

  val src : decoder -> bigstring -> int -> int -> decoder
  val dst_rem : decoder -> int
  val flush : decoder -> decoder
end
