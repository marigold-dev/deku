open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_consensus

type chain = private
  | Chain of {
      pool : Parallel.Pool.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
    }

type t = chain

type external_effect = private
  (* timer *)
  | Trigger_timeout
  (* network *)
  | Broadcast_block of { block : Block.t }
  | Broadcast_signature of { signature : Verified_signature.t }

val make :
  identity:Identity.t ->
  validators:Key_hash.t list ->
  pool:Parallel.Pool.t ->
  chain

val incoming_block :
  current:Timestamp.t -> block:Block.t -> chain -> chain * external_effect list

val incoming_signature :
  current:Timestamp.t ->
  signature:Verified_signature.t ->
  chain ->
  chain * external_effect list

val incoming_timeout :
  current:Timestamp.t -> chain -> chain * external_effect list

val incoming_operation : operation:Operation.t -> chain -> chain
