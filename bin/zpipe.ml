let () =
  Zlib.uncompress ~header:false
    (fun ibuf -> input stdin ibuf 0 (Bytes.length ibuf))
    (fun obuf len -> output stdout obuf 0 len)
