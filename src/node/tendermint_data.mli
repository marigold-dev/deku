(** Tendermint input_log and output_log.
    Holds all the querying function on the input_log required by Tendermint subprocesses. *)

open Crypto
open Tendermint_internals

type elmt = value * round

module MySet : Set.S with type elt := elmt

(* FIXME: ConsensusStep2 Tendermint we could do better *)
type node_identifier = Key_hash.t

type index_step = consensus_step

type index = height * index_step
(** Tendermint input is indexed by height and consensus step. *)

type proposal_content = private {
  process_round : round; (* Round of the process that sent the PROPOSAL *)
  proposal : value;
  process_valid_round : round;
  (* "Locked round" (see paper) of the PRECOMMIT of this value; -1 when the value has notbeen locked before *)
  sender : node_identifier;
}
(** Proposal messages carry more relevant information: the process_round, the
   proposed value, the last known valid round and, the sender *)

type prevote_content = {
  process_round : round; (* Round of the process that sent the PREVOTE *)
  repr_value : value;
  sender : node_identifier;
}
(** Prevote and Precommit messages only carry the process_round, a representation
   of the value being dealt with, and the sender. *)

type precommit_content = prevote_content

(* FIXME: ConsensusStep2 we can do better than this now that we know the requirements:
    - find a way for "tendermint processes" to register to the input log
    - find a different way of handling clocks and timeouts? *)
type content =
  | ProposalContent  of proposal_content
  | PrevoteContent   of prevote_content
  | PrecommitContent of precommit_content
  | Timeout
(* Used to trigger actions on timeouts *)

val content_of_op : node_identifier -> sidechain_consensus_op -> content

(* TODO: ConsensusStep2 ensure there is no data duplication in input_log *)
type input_log = {
  msg_log : (index, content list) Hashtbl.t;
  timeouts : (index, content) Hashtbl.t;
      (* Making timeouts a separate queue because it's easier at the moment... *)
}

val empty : unit -> input_log

val contains_timeout : input_log -> height -> consensus_step -> bool

val add : input_log -> index -> content -> input_log

val prune : input_log -> height -> unit

module OutputLog : sig
  type t = (height, value * round) Hashtbl.t

  val empty : unit -> t

  val contains_nil : t -> height -> bool

  val set : t -> height -> value -> round -> unit

  val get : t -> height -> (value * round) option

  val contains_block : t -> height -> value -> bool
end

(* Data selection *)

val count_prevotes :
  ?prevote_selection:(content -> (elmt * round) option) ->
  input_log ->
  consensus_state ->
  State.t ->
  MySet.t
(** Selects (repr_value, process_round) from Prevote data if the pair has enough
   cumulated weight.*)

val count_precommits : input_log -> consensus_state -> State.t -> MySet.t
(** Selects (repr_value, process_round) from Precommit data if the pair has
   enough cumulated weight. *)

val select_proposal_process_round_matching_proposer :
  input_log -> consensus_state -> State.t -> MySet.t
(** Selects (proposal_value, process_round) from Proposal data if the sender
   matches authorized proposer of current (height, round). *)

val select_proposal_valid_round_matching_proposer :
  input_log -> consensus_state -> State.t -> MySet.t
(** Selects (proposal_value, process_valid_round) from Proposal data if the
   sender matches authorized poposer of current (height, round). *)

val select_proposal_matching_several_rounds :
  input_log -> consensus_state -> State.t -> MySet.t
(** Selects (proposal_value, process_round) from Proposal data if the sender
   matches authorized proposer of current (height, round). *)

val count_all_actions :
  ?threshold_f:float -> input_log -> consensus_state -> State.t -> MySet.t
(* Selects (Value.nil, process_round) from Proposal, Prevote, and Precommit data
   if the pair has enough cumulated weight.
   We are not interested in getting a real value here, just checking the weight
   of all actions as this is failure detection. *)
