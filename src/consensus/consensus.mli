open Deku_concepts

type action =
  | Consensus_timeout of { from : Timestamp.t }
  | Consensus_produce of { above : Block.t }
  | Consensus_vote of { level : Level.t; vote : Verified_signature.t }
  | Consensus_apply of { block : Block.t; votes : Verified_signature.Set.t }
  | Consensus_request of { above : Level.t }

type state =
  | Propose of { finalized : Block.t }
  | Vote of { finalized : Block.t }
  | Apply of { pending : Block.t }
  | Pending_missing of { finalized : Block.t; accepted : Level.t }
  | Pending_apply of { pending : Block.t; accepted : Level.t }
  | Corrupted_stuck of { finalized : Block.t; clash : Block.t }
  | Corrupted_apply of { pending : Block.t; clash : Block.t }

type consensus =
  | Consensus of {
      identity : Identity.t;
      validators : Validators.t;
      block_pool : Block_pool.t;
      state : state;
      (* TODO: this variable is weird here *)
      accepted_at : Timestamp.t;
    }

type t = consensus [@@deriving yojson]

val make : identity:Identity.t -> validators:Validators.t -> consensus

(* updates *)
val incoming_block :
  current:Timestamp.t -> block:Block.t -> consensus -> consensus * action list

val incoming_vote :
  current:Timestamp.t ->
  (* TODO: couple level to vote hash *)
  level:Level.t ->
  vote:Verified_signature.t ->
  consensus ->
  consensus * action list

val timeout : current:Timestamp.t -> consensus -> consensus * action list

val finished :
  current:Timestamp.t ->
  block:Block.t ->
  consensus ->
  ( consensus * action list,
    [> `No_pending_block | `Wrong_pending_block ] )
  result

val test : unit -> unit
