type gossip
type t = gossip [@@deriving yojson, show]
type fragment
type outcome

type action = private
  | Gossip_apply_and_broadcast of {
      message : Message.t;
      raw_message : Message.raw;
    }
  | Gossip_send_message of {
      connection : Connection_id.t;
      raw_message : Message.raw;
    }
  | Gossip_send_request of { raw_request : Request.raw }
  | Gossip_incoming_request of {
      connection : Connection_id.t;
      request : Request.t;
    }
  | Gossip_fragment of { fragment : fragment }

val empty : gossip
val broadcast_message : content:Message.Content.t -> fragment

val incoming_message :
  raw_expected_hash:string ->
  raw_content:string ->
  gossip ->
  gossip * fragment option

val send_message :
  connection:Connection_id.t -> content:Message.Content.t -> fragment

val send_request : content:Request.Content.t -> fragment

val incoming_request :
  connection:Connection_id.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  fragment option

val apply : outcome:outcome -> gossip -> gossip * action option

val compute : fragment -> outcome
(** [compute fragment] Can be executed in parallel *)

val clear : gossip -> gossip
(** [clear gossip] To be used when restarting the chain *)
