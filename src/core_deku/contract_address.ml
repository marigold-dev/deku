open Helpers
open Crypto

type t = BLAKE2B_20.t [@@deriving eq, ord, bin_io]

let of_user_operation_hash t = BLAKE2B.to_raw_string t |> BLAKE2B_20.hash

include Encoding_helpers.Make_b58 (struct
  type nonrec t = t

  let name = "Deku_contract_hash"

  let title = "A Deku contract ID"

  let size = BLAKE2B_20.size

  let prefix = Base58.Prefix.deku_contract_hash

  let to_raw = BLAKE2B_20.to_raw_string

  let of_raw = BLAKE2B_20.of_raw_string
end)

let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "contract_hash" to_string of_string
