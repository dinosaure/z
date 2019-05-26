let () =
  match Sys.argv with
  | [| _; "-d" |] ->
    Zlib.compress ~level:4 ~header:false
      (fun ibuf -> input stdin ibuf 0 (Bytes.length ibuf))
      (fun obuf len -> output stdout obuf 0 len)
  | _ ->
    Zlib.uncompress ~header:false
      (fun ibuf -> input stdin ibuf 0 (Bytes.length ibuf))
      (fun obuf len -> output stdout obuf 0 len)
