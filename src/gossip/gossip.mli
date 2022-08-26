type gossip
type t = gossip
type fragment
type outcome

type action = private
  | Gossip_apply_and_broadcast of {
      message : Message.t;
      raw_message : Message.raw;
    }
  | Gossip_fragment of { fragment : fragment }

val empty : gossip

val incoming :
  raw_expected_hash:string ->
  raw_content:string ->
  gossip ->
  gossip * fragment option
(** [incoming ~raw_expected_hash ~raw_content gossip] *)

val broadcast : content:Message.Content.t -> fragment
(** [broadcast ~content gossip] *)

val apply : outcome:outcome -> gossip -> gossip * action option
(** [apply ~compute gossip] *)

val compute : fragment -> outcome
(** [compute fragment] Can be executed in parallel *)
