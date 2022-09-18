open Deku_crypto
open Deku_concepts

type action = private
  (* protocol *)
  | Consensus_accepted_block of { block : Block.t }
  (* timer *)
  | Consensus_trigger_timeout of { level : Level.t }
  (* network *)
  | Consensus_broadcast_vote of { vote : Verified_signature.t }
  | Consensus_request_block of { self : Key_hash.t; hash : Block_hash.t }

type consensus = private
  | Consensus of {
      (* state *)
      current_block : Block.t;
      last_update : Timestamp.t option;
      (* consensus *)
      identity : Identity.t;
      validators : Validators.t;
      accepted : Block_hash.Set.t;
      block_pool : Block_pool.t;
    }

and t = consensus

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
