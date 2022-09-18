open Deku_crypto

type gossip
type t = gossip
type fragment
type outcome

type action = private
  | Gossip_apply_and_broadcast of {
      message : Message.t;
      raw_message : Message.raw;
    }
  | Gossip_send of { to_ : Key_hash.t; raw_message : Message.raw }
  | Gossip_fragment of { fragment : fragment }

(* TODO: Timestamp.t *)
type timestamp = float

val empty : gossip

val incoming :
  raw_expected_hash:string ->
  raw_content:string ->
  gossip ->
  gossip * fragment option
(** [incoming ~raw_expected_hash ~raw_content gossip] *)

val send : to_:Key_hash.t -> content:Message.Content.t -> fragment
(** [send ~content gossip] *)

val broadcast : content:Message.Content.t -> fragment
(** [broadcast ~content gossip] *)

val apply :
  current:timestamp -> outcome:outcome -> gossip -> gossip * action option
(** [apply ~compute gossip] *)

val compute : fragment -> outcome
(** [compute fragment] Can be executed in parallel *)

(* TODO: this is weird *)
val clean : current:timestamp -> gossip -> gossip

(* TODO: remove this *)
val test : unit -> unit
