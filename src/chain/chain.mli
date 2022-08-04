open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_gossip

type chain = private
  | Chain of {
      pool : Parallel.Pool.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
    }

type t = chain

type action = private
  | Chain_trigger_timeout
  | Chain_broadcast of { content : Message.Content.t }

val make :
  identity:Identity.t ->
  bootstrap_key:Key.t ->
  validators:Key_hash.t list ->
  pool:Parallel.Pool.t ->
  chain

val incoming_message :
  current:Timestamp.t -> message:Message.t -> chain -> chain * action list
(** [incoming ~current ~message chain] *)

val incoming_timeout : current:Timestamp.t -> chain -> chain * action list
(** [incoming_timeout ~current chain] *)
