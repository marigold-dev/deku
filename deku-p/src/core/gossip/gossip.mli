open Deku_concepts

type gossip
type t = gossip [@@deriving yojson]
type fragment
type outcome

type action = private
  | Gossip_apply_and_broadcast of {
      content : Message.Content.t;
      network : Message.Network.t;
    }
  | Gossip_send_message of {
      connection : Connection_id.t;
      network : Message.Network.t;
    }
  | Gossip_incoming_request of { connection : Connection_id.t; above : Level.t }
  | Gossip_fragment of { fragment : fragment }

val encoding : gossip Data_encoding.t
val initial : gossip
val broadcast_message : content:Message.Content.t -> fragment

val incoming_message :
  raw_header:string -> raw_content:string -> gossip -> gossip * fragment option

val send_message :
  connection:Connection_id.t -> content:Message.Content.t -> fragment

val send_request : above:Level.t -> gossip -> gossip * Request.Network.t option

val incoming_request :
  connection:Connection_id.t ->
  raw_header:string ->
  raw_content:string ->
  fragment

val apply : outcome:outcome -> gossip -> gossip * action option

val compute : fragment -> outcome
(** [compute fragment] Can be executed in parallel *)

val close : until:Level.t -> gossip -> gossip
