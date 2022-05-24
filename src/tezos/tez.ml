open Helpers

type t = int64 [@@deriving eq, ord]

let encoding =
  let open Data_encoding in
  let name = "mutez" in
  def name (check_size 10 (conv Z.of_int64 (Json.wrap_error Z.to_int64) n))

let to_yojson, of_yojson = Yojson_ext.with_data_encoding encoding

let zero = 0L

let one_mutez = 1L

let one = 1_000_000L

let of_mutez t = if t < 0L then None else Some t
