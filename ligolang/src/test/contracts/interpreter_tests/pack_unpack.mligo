
let test_string =
  let data = "Coucou" in
  let packed = Bytes.pack data in
  let unpacked : string option = Bytes.unpack packed in
  assert (Some data = unpacked)

let test_int =
  let data = 42 in
  let packed = Bytes.pack data in
  let unpacked : int option = Bytes.unpack packed in
  assert (Some data = unpacked)

let test_string_int =
  let data = "Coucou" in
  let packed = Bytes.pack data in
  let unpacked : int option = Bytes.unpack packed in
  assert ((None : int option) = unpacked)

let test_string_string =
  let data = "Coucou" in
  let packed = Bytes.pack data in
  let packed = Bytes.concat packed packed in
  let unpacked : string option = Bytes.unpack packed in
  assert ((None : string option) = unpacked)
