open Deku_concepts
open Deku_crypto

type action = private
  (* protocol *)
  | Consensus_accepted_block of Block.t
  (* timer *)
  | Consensus_trigger_timeout of { level : Level.t }
  (* network *)
  | Consensus_broadcast_signature of { signature : Verified_signature.t }
[@@deriving show]

type consensus = private
  | Consensus of {
      block_pool : Block_pool.t;
      signer : Signer.t;
      state : State.t;
      bootstrap_key : Key.t;
    }

and t = consensus

val make :
  identity:Identity.t ->
  validators:Validators.t ->
  bootstrap_key:Key.t ->
  consensus

(* updates *)
val incoming_block :
  current:Timestamp.t -> block:Block.t -> consensus -> consensus * action list

val incoming_signature :
  current:Timestamp.t ->
  signature:Verified_signature.t ->
  consensus ->
  consensus * action list

val incoming_bootstrap_signal :
  bootstrap_signal:Bootstrap_signal.t ->
  current:Timestamp.t ->
  consensus ->
  consensus option
