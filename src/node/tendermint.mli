open Tendermint_helpers
open Tendermint_processes
open Tendermint_internals
open Tendermint_data

type t = {
  identity : State.identity;
  clocks : clock list IntSet.t;
  consensus_states : consensus_state IntSet.t;
  (* FIXME: ConsensusStep2 we don't want this to be mutable *)
  mutable procs : (height * process) list;
  node_state : State.t;
  input_log : input_log;
  output_log : OutputLog.t;
}
(** Tendermint simplification: as Deku is not going to run several
   blockchain heights at the same time, we only consider one set of
   states and clocks. *)

val make : State.t -> height -> t

val is_valid_consensus_op :
  State.t -> sidechain_consensus_op -> (unit, string) result

val add_consensus_op : t -> node_identifier -> sidechain_consensus_op -> t Lwt.t

val exec_consensus : t -> t

val make_proposal :
  height -> round -> Protocol.Block.t -> sidechain_consensus_op

val get_block_opt : t -> height -> (Protocol.Block.t * round) option
(** Required to publish hash on Tezos *)
