open Deku_crypto
open Deku_repr
open BLAKE2b
open BLAKE2b_160

type t = BLAKE2b_160.t [@@deriving eq, ord, show]

let of_user_operation_hash t = BLAKE2b_256.to_hex t |> BLAKE2b_160.hash

include With_b58_and_encoding_and_yojson (struct
  let name = "Deku_contract_hash"
  let prefix = Prefix.deku_contract_hash
end)

let encoding =
  let name = "Deku_protocol.Contract_address" in
  make_encoding ~name ~to_string:to_b58 ~of_string:of_b58 ~raw_encoding:encoding
