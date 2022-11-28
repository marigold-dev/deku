open Deku_stdlib
open Deku_crypto

module Consensus : sig
  val hash_validators : Key_hash.t list -> BLAKE2b.t
  val hash_packed_data : Tezos_pack.t -> BLAKE2b.t
  val hash : BLAKE2b.t -> Tezos_pack.t

  val hash_block :
    block_level:N.t ->
    block_payload_hash:BLAKE2b.t ->
    state_root_hash:BLAKE2b.t ->
    withdrawal_handles_hash:BLAKE2b.t ->
    BLAKE2b.t

  val hash_withdrawal_handle :
    id:int ->
    owner:Tezos_contract.t ->
    amount:N.t ->
    ticket_id:Tezos_ticket_id.t ->
    BLAKE2b.t
end

module Ticket_data : sig
  val encode : ticketer:bytes -> data:bytes -> bytes
  val decode : bytes -> (bytes * bytes) option
end
