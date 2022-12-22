let alphabet =
  Base64.make_alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+_"

let make content nonce =
  let nonce =
    nonce |> string_of_int |> Hex.of_string |> Hex.to_bytes |> Cstruct.of_bytes
  in
  let content = Cstruct.of_string content in
  Cstruct.append content nonce
  |> Mirage_crypto.Hash.MD5.digest |> Cstruct.to_string
  |> Base64.encode_string ~pad:false ~alphabet
