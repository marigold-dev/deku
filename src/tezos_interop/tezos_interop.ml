open Helpers
open Crypto
open Tezos
module Context = Consensus_contract_interop.Context
module Consensus = Consensus_contract_interop.Consensus
module Discovery = struct
  open Pack
  let sign secret ~nonce uri =
    to_bytes
      (pair
         (int (Z.of_int64 nonce))
         (bytes (Bytes.of_string (Uri.to_string uri))))
    |> Bytes.to_string
    |> BLAKE2B.hash
    |> Signature.sign secret
end
