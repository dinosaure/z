type fd = Unix.file_descr

external bigstring_input : fd -> Z.bigstring -> int -> int -> int = "bigstring_read"
external bigstring_output : fd -> Z.bigstring -> int -> int -> unit = "bigstring_write"
