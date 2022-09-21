open Deku_concepts

type action = private
  (* protocol *)
  | Consensus_accepted_block of { block : Block.t }
  (* timer *)
  | Consensus_trigger_timeout of { level : Level.t }
  (* network *)
  | Consensus_broadcast_vote of { vote : Verified_signature.t }
  | Consensus_request_block of { hash : Block_hash.t }
[@@deriving show]

type consensus = private
  | Consensus of {
      (* state *)
      current_block : Block.t;
      last_update : Timestamp.t;
      (* consensus *)
      identity : Identity.t;
      validators : Validators.t;
      accepted : Block_hash.Set.t;
      block_pool : Block_pool.t;
    }

type t = consensus [@@deriving yojson]

val make : identity:Identity.t -> validators:Validators.t -> consensus

(* helpers *)
val is_producer : current:Timestamp.t -> consensus -> bool

(* updates *)
val incoming_block :
  current:Timestamp.t -> block:Block.t -> consensus -> consensus * action list

val incoming_vote :
  current:Timestamp.t ->
  vote:Verified_signature.t ->
  consensus ->
  consensus * action list
