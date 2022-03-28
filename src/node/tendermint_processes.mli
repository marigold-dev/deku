open Tendermint_internals
open Tendermint_data

module type Clock_type = sig
  type 'a t = {
    time : int;
    step : consensus_step;
    height : height;
    round : round;
    on_timeout : consensus_state -> input_log -> OutputLog.t -> State.t -> 'a;
    started : bool;
  }
  val make :
    int ->
    consensus_step ->
    height ->
    round ->
    (consensus_state -> input_log -> OutputLog.t -> State.t -> 'a) ->
    'a t
end

module Clock : Clock_type

(* Values returned by processes, to be interpreted by the node as network
   s *)
type consensus_action =
  | Broadcast         of sidechain_consensus_op
  | Schedule          of consensus_action option Clock.t
  | RestartTendermint of (height * round)
  | DoNothing

type process =
  height ->
  round ->
  consensus_state ->
  input_log ->
  OutputLog.t ->
  State.t ->
  consensus_action option
(** Tendermint processes *)

type clock = consensus_action option Clock.t
(** Clocks scheduled for timeouts, to be used in Tendermint *)

val all_processes : process list
