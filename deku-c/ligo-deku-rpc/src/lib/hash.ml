let alphabet =
  Base64.make_alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+_"

let make content =
  let content = Cstruct.of_string content in
  Mirage_crypto.Hash.MD5.digest content
  |> Cstruct.to_string
  |> Base64.encode_string ~pad:false ~alphabet