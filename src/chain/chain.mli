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
      applied :
        (Block.block * Verified_signature.verified_signature list) Level.Map.t;
    }

type t = chain [@@deriving yojson]
type fragment
type outcome

type action = private
  | Chain_timeout of { from : Timestamp.t }
  | Chain_broadcast of { raw_expected_hash : string; raw_content : string }
  | Chain_send_message of {
      connection : Connection_id.t;
      raw_expected_hash : string;
      raw_content : string;
    }
  | Chain_send_request of { raw_expected_hash : string; raw_content : string }
  | Chain_fragment of { fragment : fragment }

val make : validators:Key_hash.t list -> chain

val incoming :
  raw_expected_hash:string ->
  raw_content:string ->
  chain ->
  chain * fragment option
(** [incoming ~raw_expected_hash ~raw_content chain] *)

val request :
  connection:Connection_id.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  fragment option
(** [request ~id ~raw_expected_hash ~raw_content chain] *)

val timeout :
  identity:Identity.t -> current:Timestamp.t -> chain -> chain * action list
(** [incoming_timeout ~current chain] *)

val apply :
  identity:Identity.t ->
  current:Timestamp.t ->
  outcome:outcome ->
  chain ->
  chain * action list
(** [apply ~current ~outcome chain ]*)

val compute : identity:Identity.t -> fragment -> outcome
(** [compute fragment] Can be executed in parallel *)

val clear : chain -> chain
(** [clear chain] To be used when restarting the chain *)

(* TODO: remove this in the future *)
val test : unit -> unit
