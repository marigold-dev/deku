open Crypto
module Consensus : sig
  val hash_validators : Key_hash.t list -> BLAKE2B.t
  val hash_block :
    block_height:int64 ->
    block_payload_hash:BLAKE2B.t ->
    state_root_hash:BLAKE2B.t ->
    withdrawal_handles_hash:BLAKE2B.t ->
    validators_hash:BLAKE2B.t ->
    BLAKE2B.t
  val hash_withdraw_handle :
    id:Z.t ->
    owner:Address.t ->
    amount:Z.t ->
    ticketer:Address.t ->
    data:bytes ->
    BLAKE2B.t
end
