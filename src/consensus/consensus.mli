open Deku_concepts

type external_effect = private
  (* protocol *)
  | Accepted_block of { level : Level.t; payload : string list }
  (* timer *)
  | Trigger_timeout
  (* network *)
  | Broadcast_signature of { signature : Verified_signature.t }

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
  current:Timestamp.t ->
  block:Block.t ->
  consensus ->
  consensus * external_effect list

val incoming_signature :
  current:Timestamp.t ->
  signature:Verified_signature.t ->
  consensus ->
  consensus * external_effect list
