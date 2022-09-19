open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_gossip
open Deku_external_vm

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
  | Chain_save_block of Block.t
[@@deriving show]

val make :
  identity:Identity.t ->
  bootstrap_key:Key.t ->
  validators:Key_hash.t list ->
  pool:Parallel.Pool.t ->
  default_block_size:int ->
  vm_state:External_vm_protocol.State.t ->
  chain

val incoming_message :
  current:Timestamp.t ->
  content:Message.Content.t ->
  chain ->
  chain * action list
(** [incoming ~current ~message chain] *)

val incoming_timeout : current:Timestamp.t -> chain -> chain * action list
(** [incoming_timeout ~current chain] *)

val incoming_tezos_operation :
  tezos_operation:Tezos_operation.t -> chain -> chain * action list
(** [incoming_tezos_operation ~tezos_operation chain] *)

(* TODO: remove this in the future *)
val test : unit -> unit
