open Crypto

type height = int64 [@@deriving yojson]

type round = int [@@deriving yojson]
(** At a specific (chain) height, Tendermint's consensus algorithm may run several rounds.*)

(** Tendermint sometimes decides on a `Nil` value. *)
type value =
  | Block of Protocol.Block.t
  | Nil
[@@deriving yojson]

val repr_of_value : value -> value
(* TODO: ConsensusStep2 Tendermint uses repr of values instead of values. *)

(* FIXME: ConsensusStep2 this is copied bad design. *)
val produce_value : (State.t -> value) ref

val is_valid : State.t -> value -> bool
(** Ensures a value going through Tendermint is valid. TODO: ConsensusStep2 actually verify signatures, logging system. *)

val on_block : nil_default:'a -> (Protocol.Block.t -> 'a) -> value -> 'a

val block : Protocol.Block.t -> value

val nil : value

val update_value : value -> round -> value

type consensus_step =
  | Proposal
  | Prevote
  | Precommit
      (** Tendermint's consensus goes through 3 steps at each round. Used inside a consensus instance in a node. *)

val string_of_step : consensus_step -> string

(** Tendermint's consensus step-communication with other nodes. Two first fields are height and
    round of when the operation was sent. *)
type sidechain_consensus_op =
  | ProposalOP  of (height * round * value * round)
  | PrevoteOP   of (height * round * value)
  | PrecommitOP of (height * round * value)
[@@deriving yojson]

val step_of_op : sidechain_consensus_op -> consensus_step

val string_of_op : sidechain_consensus_op -> string

val height : sidechain_consensus_op -> height

val round : sidechain_consensus_op -> round

val value_of_op : sidechain_consensus_op -> value

val hash_of_value : value -> BLAKE2B.t

val hash_of_consensus_op : sidechain_consensus_op -> Key_hash.t -> BLAKE2B.t
(** Hash of a consensus_op (including the step) + sender. *)

val hash_of_consensus_value : sidechain_consensus_op -> BLAKE2B.t
(** Hash of a value+height+round. If the value is not Nil, returns the state_root_hash of the block.
    Unlike hash_of_consensus_op, does not discriminate the steps. *)

(* TODO: ConsensusStep2 remove mutable. *)
type consensus_state = {
  mutable height : height;
  mutable round : round;
  mutable step : consensus_step;
  mutable locked_value : value;
  mutable locked_round : round;
  mutable valid_value : value;
  mutable valid_round : round;
}

val fresh_state : height -> consensus_state

val short : Key_hash.t -> string

val debug : State.t -> string -> unit

val is_allowed_proposer : State.t -> height -> round -> Key_hash.t -> bool
(** Verifies that the Key_hash.t matches the one expected by protocol for this height and round. *)

val i_am_proposer : State.t -> height -> round -> bool
(** True if the node is the proposer for this (height, round) according to proposer selection. *)

val get_weight : State.t -> Key_hash.t -> int
(** Implemented as Proof-of-Authority. *)

val proposal_timeout : int
val prevote_timeout : int
val precommit_timeout : int
