open Deku_crypto
open Deku_concepts

module Consensus : sig
  val hash_validators : Key_hash.t list -> BLAKE2b.t
  val hash_packed_data : Pack.t -> BLAKE2b.t
  val hash : BLAKE2b.t -> Pack.t

  val hash_block :
    block_level:Level.t ->
    block_payload_hash:BLAKE2b.t ->
    state_root_hash:BLAKE2b.t ->
    withdrawal_handles_hash:BLAKE2b.t ->
    BLAKE2b.t

  val hash_withdrawal_handle :
    id:Z.t ->
    owner:Address.t ->
    amount:Z.t ->
    ticketer:Address.t ->
    data:bytes ->
    BLAKE2b.t
  
    val hash_withdrawal_handle_deku :
    id:Z.t ->
    owner:Address.t ->
    amount:Z.t ->
    ticketer:string->
    data:bytes ->
    BLAKE2b.t
end
