open Crypto
open Helpers

type t = BLAKE2B.t [@@deriving eq, ord]

include Encoding_helpers.Make_b58 (struct
  type nonrec t = t
  let name = "Protocol_hash"

  let title = "A Tezos protocol ID"

  let size = BLAKE2B.size

  let prefix = Base58.Prefix.protocol_hash

  let to_raw = BLAKE2B.to_raw_string

  let of_raw = BLAKE2B.of_raw_string
end)

let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "Protocol_hash" to_string of_string
