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
      applied : Block.t Block_hash.Map.t;
    }

type t = chain [@@deriving yojson]
type fragment
type outcome

type action = private
  | Chain_trigger_timeout
  | Chain_broadcast of { raw_expected_hash : string; raw_content : string }
  | Chain_send_request of { raw_expected_hash : string; raw_content : string }
  | Chain_send_response of {
      id : Request_id.t;
      raw_expected_hash : string;
      raw_content : string;
    }
  | Chain_send_not_found of { id : Request_id.t }
  | Chain_fragment of { fragment : fragment }

val make : identity:Identity.t -> validators:Key_hash.t list -> chain

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

val apply :
  current:Timestamp.t -> outcome:outcome -> chain -> chain * action list
(** [apply ~current ~outcome chain ]*)

val compute : fragment -> outcome
(** [compute fragment] Can be executed in parallel *)

val clear : chain -> chain
(** [clear chain] To be used when restarting the chain *)

(* TODO: remove this in the future *)
val test : unit -> unit
