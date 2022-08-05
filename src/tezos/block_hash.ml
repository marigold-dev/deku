open Crypto
open Helpers

type t = BLAKE2B.t [@@deriving eq, ord]

include Encoding_helpers.Make_b58 (struct
  type nonrec t = t

  let name = "block_hash"
  let title = "A block identifier"
  let size = BLAKE2B.size
  let prefix = Base58.Prefix.block_hash
  let to_raw = BLAKE2B.to_raw_string
  let of_raw = BLAKE2B.of_raw_string
end)

let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "block_hash" to_string of_string
