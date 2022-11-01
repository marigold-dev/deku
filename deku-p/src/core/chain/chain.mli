open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_gossip

type chain = private
  | Chain of {
      gossip : Gossip.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
      oldest_trusted : Level.t;
      trusted : Message.Network.t Level.Map.t;
    }

type t = chain [@@deriving yojson]
type fragment
type outcome

type action = private
  | Chain_timeout of { until : Timestamp.t }
  | Chain_broadcast of { raw_header : string; raw_content : string }
  | Chain_send_message of {
      connection : Connection_id.t;
      raw_header : string;
      raw_content : string;
    }
  | Chain_send_request of { raw_header : string; raw_content : string }
  | Chain_fragment of { fragment : fragment }
  | Chain_save_block of { block : Block.t }
  | Chain_commit of {
      current_level : Level.t;
      payload_hash : BLAKE2b.t;
      state_root_hash : BLAKE2b.t;
      signatures : (Key.t * Signature.t) option list;
      validators : Key_hash.t list;
      withdrawal_handles_hash : Deku_ledger.Ledger.Withdrawal_handle.hash;
    }
[@@deriving show]

val make : validators:Key_hash.t list -> vm_state:Ocaml_wasm_vm.State.t -> chain

val incoming :
  raw_header:string -> raw_content:string -> chain -> chain * fragment option
(** [incoming ~raw_expected_hash ~raw_content chain] *)

val request :
  connection:Connection_id.t ->
  raw_header:string ->
  raw_content:string ->
  fragment
(** [request ~id ~raw_expected_hash ~raw_content chain] *)

val timeout :
  identity:Identity.t -> current:Timestamp.t -> chain -> chain * action list
(** [incoming_timeout ~current chain] *)

val incoming_tezos_operation :
  tezos_operation:Tezos_operation.t -> chain -> chain * action list
(** [incoming_tezos_operation ~tezos_operation chain] *)

val apply :
  identity:Identity.t ->
  current:Timestamp.t ->
  outcome:outcome ->
  chain ->
  chain * action list
(** [apply ~current ~outcome chain ]*)

val compute :
  identity:Identity.t -> default_block_size:int -> fragment -> outcome
(** [compute fragment] Can be executed in parallel *)

val reload : current:Timestamp.t -> chain -> chain * action list
(** [reload chain] To be used when restarting the chain *)

(* TODO: remove this in the future *)
val test : unit -> unit
