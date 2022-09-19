open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_consensus

type chain = private
  | Chain of {
      protocol : Protocol.t;
      consensus : Consensus.t;
      verifier : Verifier.t;
      signer : Signer.t;
      producer : Producer.t;
    }

type t = chain

type external_effect = private
  | Reset_timeout
  | Broadcast_block of Block.t
  | Broadcast_signature of Verified_signature.t
  | Save_block of Block.t
  | Commit_state_hash of {
      current_level : Level.t;
      payload_hash : BLAKE2b.t;
      state_root_hash : BLAKE2b.t;
      withdrawal_handles_hash : Deku_protocol.Ledger.Withdrawal_handle.hash;
      signatures : (Key.t * Signature.t) option list;
      validators : Key_hash.t list;
    }

val make :
  identity:Identity.t ->
  bootstrap_key:Key.t ->
  validators:Key_hash.t list ->
  chain

val incoming_block :
  pool:Parallel.Pool.pool ->
  current:Timestamp.timestamp ->
  block:Block.block ->
  chain ->
  chain * external_effect list

val incoming_signature :
  pool:Parallel.Pool.pool ->
  current:Timestamp.timestamp ->
  signature:Verified_signature.verified_signature ->
  chain ->
  chain * external_effect list

val incoming_timeout :
  pool:Deku_stdlib.Parallel.Pool.pool ->
  current:Timestamp.timestamp ->
  chain ->
  chain * external_effect list

val incoming_operation : operation:Operation.operation -> chain -> chain

val incoming_bootstrap_signal :
  pool:Deku_stdlib.Parallel.Pool.pool ->
  bootstrap_signal:Bootstrap_signal.t ->
  current:Timestamp.timestamp ->
  chain ->
  chain * external_effect list

val incoming_tezos_operation :
  tezos_operation:Tezos_operation.t -> chain -> chain * external_effect list
