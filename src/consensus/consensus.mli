open Deku_concepts

type action = private
  (* protocol *)
  | Consensus_accepted_block of { level : Level.t; payload : string list }
  (* timer *)
  | Consensus_trigger_timeout
  (* network *)
  | Consensus_broadcast_signature of { signature : Verified_signature.t }

type consensus = private
  | Consensus of {
      block_pool : Block_pool.t;
      signer : Signer.t;
      state : State.t;
    }

and t = consensus

val make : identity:Identity.t -> validators:Validators.t -> consensus

(* updates *)
val incoming_block :
  current:Timestamp.t -> block:Block.t -> consensus -> consensus * action list

val incoming_signature :
  current:Timestamp.t ->
  signature:Verified_signature.t ->
  consensus ->
  consensus * action list
