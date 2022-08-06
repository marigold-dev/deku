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

val make : identity:Identity.t -> validators:Key_hash.t list -> chain

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
  current:Timestamp.timestamp -> chain -> chain * external_effect list

val incoming_operation : operation:Operation.operation -> chain -> chain
