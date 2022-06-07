open Crypto

type t = private {
  hash : BLAKE2B.t;
  payload_hash : BLAKE2B.t;
  state_root_hash : BLAKE2B.t;
  withdrawal_handles_hash : BLAKE2B.t;
  validators_hash : BLAKE2B.t;
  previous_hash : BLAKE2B.t;
  author : Key_hash.t;
  block_height : int64;
  consensus_operations : Protocol_operation.Consensus.t list;
  tezos_operations : Protocol_operation.Core_tezos.t list;
  user_operations : Protocol_operation.Core_user.t list;
}
[@@deriving yojson, ord]

val sign : key:Secret.t -> t -> Protocol_signature.t

val genesis : t

val produce :
  state:Protocol_state.t ->
  next_state_root_hash:BLAKE2B.t option ->
  author:Key_hash.t ->
  consensus_operations:Protocol_operation.Consensus.t list ->
  tezos_operations:Protocol_operation.Core_tezos.t list ->
  user_operations:Protocol_operation.Core_user.t list ->
  t
