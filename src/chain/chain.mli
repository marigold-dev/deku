open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_gossip
open Deku_external_vm

type chain_data = private
  | Chain_data of {
      gossip : Gossip.t;
      protocol : Protocol.t;
      consensus : Consensus.consensus_data;
      producer : Producer.producer_data;
      applied : Block.t Block_hash.Map.t;
    }
[@@deriving yojson]

type chain = private
  | Chain of {
      gossip : Gossip.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
      applied : Block.t Block_hash.Map.t;
    }

type t = chain
type fragment
type outcome

val rehydrate : identity:Identity.t -> default_block_size:int -> chain_data -> t
val dehydrate : t -> chain_data

val add_block :
  block:Block.t -> block_timestamp:Timestamp.t -> chain_data -> chain_data

type action = private
  | Chain_trigger_timeout
  | Chain_broadcast of { raw_expected_hash : string; raw_content : string }
  | Chain_save_block of Block.t
  | Chain_send_request of { raw_expected_hash : string; raw_content : string }
  | Chain_send_response of {
      id : Request_id.t;
      raw_expected_hash : string;
      raw_content : string;
    }
  | Chain_send_not_found of { id : Request_id.t }
  | Chain_fragment of { fragment : fragment }
  | Chain_commit of {
      current_level : Level.t;
      payload_hash : BLAKE2b.t;
      state_root_hash : BLAKE2b.t;
      signatures : (Key.t * Signature.t) option list;
      validators : Key_hash.t list;
      withdrawal_handles_hash : Deku_protocol.Ledger.Withdrawal_handle.hash;
    }
[@@deriving show]

val make :
  identity:Identity.t ->
  validators:Key_hash.t list ->
  default_block_size:int ->
  vm_state:External_vm_protocol.State.t ->
  chain

val incoming :
  raw_expected_hash:string ->
  raw_content:string ->
  chain ->
  chain * fragment option
(** [incoming ~raw_expected_hash ~raw_content chain] *)

val request :
  id:Request_id.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  chain ->
  chain * fragment option
(** [request ~id ~raw_expected_hash ~raw_content chain] *)

val response :
  raw_expected_hash:string ->
  raw_content:string ->
  chain ->
  chain * fragment option
(** [response ~id ~raw_expected_hash ~raw_content chain] *)

val timeout : current:Timestamp.t -> chain -> fragment option
(** [incoming_timeout ~current chain] *)

val incoming_tezos_operation :
  tezos_operation:Tezos_operation.t -> chain -> chain * action list
(** [incoming_tezos_operation ~tezos_operation chain] *)

val apply :
  current:Timestamp.t -> outcome:outcome -> chain -> chain * action list
(** [apply ~current ~outcome chain ]*)

val compute : fragment -> outcome
(** [compute fragment] Can be executed in parallel *)

(* TODO: remove this in the future *)
val test : unit -> unit
