type fd = Unix.file_descr

external bigstring_input : fd -> Dd.bigstring -> int -> int -> int = "bigstring_read"
external bigstring_output : fd -> Dd.bigstring -> int -> int -> unit = "bigstring_write"
