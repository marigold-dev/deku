type payload = Payload of string list
type t = payload

let encoding = Data_encoding.(list string)

let encode ~payload =
  let (Payload payload) = payload in
  Data_encoding.Binary.to_string_exn encoding payload

let decode ~payload =
  let payload = Data_encoding.Binary.of_string_exn encoding payload in
  Payload payload
