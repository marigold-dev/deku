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
  consensus_round : int;
  operations : Protocol_operation.t list;
}
[@@deriving yojson, ord]
val sign : key:Secret.t -> t -> Protocol_signature.t
val verify : signature:Protocol_signature.t -> t -> bool
val genesis : t

val update_round : t -> consensus_round:int -> t
(** [update_round block round] changes the value of [block.consensus_round] and recomputes the hash
    *)

val produce :
  state:Protocol_state.t ->
  next_state_root_hash:BLAKE2B.t option ->
  author:Key_hash.t ->
  operations:Protocol_operation.t list ->
  t
