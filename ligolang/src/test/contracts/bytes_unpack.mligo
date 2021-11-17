let id_string (p : string) : string option =
  let packed : bytes = Bytes.pack p in
  (Bytes.unpack packed : string option)

let id_int (p : int) : int option =
  let packed : bytes = Bytes.pack p in
  (Bytes.unpack packed : int option)

let id_address (p : address) : address option =
  let packed : bytes = Bytes.pack p in
  (Bytes.unpack packed : address option)
