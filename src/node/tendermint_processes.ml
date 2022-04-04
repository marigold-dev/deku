(** Tendermint's consensus processes as describded at https://arxiv.org/pdf/1807.04938.pdf *)

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

module Clock : Clock_type = struct
  type 'a t = {
    time : int;
    step : consensus_step;
    height : height;
    round : round;
    on_timeout : consensus_state -> input_log -> OutputLog.t -> State.t -> 'a;
    started : bool;
  }

  let make time step height round on_timeout =
    { time; step; on_timeout; started = false; height; round }
end

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

type clock = consensus_action option Clock.t

let on_timeout_propose (height : height) (round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog
    _global_state =
  let do_something consensus_state =
    if
      height = consensus_state.height
      && round = consensus_state.round
      && consensus_state.step = Proposal
    then (
      consensus_state.step <- Prevote;
      Broadcast (PrevoteOP (consensus_state.height, consensus_state.round, nil)))
    else
      DoNothing in
  if contains_timeout msg_log height Proposal then
    Some (do_something consensus_state)
  else
    None

let on_timeout_prevote (height : height) (round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog
    _global_state =
  let do_something consensus_state =
    if
      height = consensus_state.height
      && round = consensus_state.round
      && consensus_state.step = Prevote
    then (
      consensus_state.step <- Precommit;
      Broadcast
        (PrecommitOP (consensus_state.height, consensus_state.round, nil)))
    else
      DoNothing in
  if contains_timeout msg_log height Prevote then
    Some (do_something consensus_state)
  else
    None

let start_round (height : height) (round : round)
    (consensus_state : consensus_state) (_msg_log : input_log) _dlog
    global_state =
  consensus_state.round <- round;
  consensus_state.step <- Proposal;
  (* If we start a round > 0 and already have a valid/locked value, we update their hash
     with the correct round *)
  (if consensus_state.valid_value <> nil then
     let new_value = update_value consensus_state.valid_value round in
     consensus_state.valid_value <- new_value);
  (if consensus_state.locked_value <> nil then
     let new_value = update_value consensus_state.locked_value round in
     consensus_state.locked_value <- new_value);
  let return_action =
    if i_am_proposer global_state height round then
      if consensus_state.valid_value <> nil then
        Some
          (Broadcast
             (ProposalOP
                ( consensus_state.height,
                  consensus_state.round,
                  consensus_state.valid_value,
                  consensus_state.valid_round )))
      else
        Some
          (Broadcast
             (ProposalOP
                ( consensus_state.height,
                  consensus_state.round,
                  (* FIXME: remove depdendency to full global_state *)
                  !produce_value global_state,
                  consensus_state.valid_round )))
    else
      Some
        (Schedule
           (Clock.make proposal_timeout Proposal height round
              (on_timeout_propose height round))) in
  return_action

let on_timeout_precommit (height : height) (round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog
    _global_state =
  let do_something consensus_state =
    if height = consensus_state.height && round = consensus_state.round then
      RestartTendermint (height, round + 1)
    else
      DoNothing in
  if contains_timeout msg_log height Precommit then
    Some (do_something consensus_state)
  else
    None

let response_to_proposal (height : height) (round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog global_state
    =
  let _height = consensus_state.height in
  let step = consensus_state.step in
  let _round = consensus_state.round in
  let do_something (consensus_state : consensus_state) (found_set : MySet.t) =
    let value, _ = MySet.choose found_set in
    consensus_state.step <- Prevote;
    if
      is_valid global_state value
      && (consensus_state.locked_round = -1
         || consensus_state.locked_value = value)
    then
      Broadcast (PrevoteOP (height, round, repr_of_value value))
    else
      Broadcast (PrevoteOP (height, round, nil)) in
  let found_set =
    select_proposal_process_round_matching_proposer msg_log consensus_state
      global_state in
  let extract_valid_set (found_set : MySet.t) =
    MySet.filter (fun (_, r) -> r = round) found_set in
  let valid_set = extract_valid_set found_set in
  if valid_set = MySet.empty || step <> Proposal then
    None
  else
    Some (do_something consensus_state valid_set)

let precommit_failed_previous_round (_height : height) (_round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog global_state
    =
  let height = consensus_state.height in
  let step = consensus_state.step in
  let round = consensus_state.round in
  (* Returns list of (repr_value, round) with filtered round *)
  let extract_common_set (left_set : MySet.t) (right_set : MySet.t) : MySet.t =
    let filtered_left_set =
      MySet.filter (fun (_, r) -> r >= 0 && r < round) left_set in
    (* (v, r)*)
    let filtered_right_set =
      MySet.filter (fun (_, r) -> r >= 0 && r < round) right_set in
    (* (rv, r)*)
    MySet.filter
      (fun (v, r) -> MySet.mem (repr_of_value v, r) filtered_right_set)
      filtered_left_set in
  let do_something (consensus_state : consensus_state) (common_set : MySet.t) =
    let value, valid_round = MySet.choose common_set in
    consensus_state.step <- Prevote;
    if
      is_valid global_state value
      && (consensus_state.locked_round <= valid_round
         || consensus_state.locked_value = value)
    then
      Broadcast (PrevoteOP (height, round, repr_of_value value))
    else
      Broadcast (PrevoteOP (height, round, nil)) in
  let left_set =
    select_proposal_valid_round_matching_proposer msg_log consensus_state
      global_state in
  let right_set = count_prevotes msg_log consensus_state global_state in
  let common_set = extract_common_set left_set right_set in
  if
    left_set = MySet.empty
    || right_set = MySet.empty
    || step <> Proposal
    || common_set = MySet.empty
  then
    None
  else
    Some (do_something consensus_state common_set)

let prepare_default_precommit_prevote_phase (height : height) (round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog global_state
    =
  let take_all_prevotes = function
    | PrevoteContent content when content.process_round = consensus_state.round
      ->
      Some ((nil, content.process_round), get_weight global_state content.sender)
    | PrevoteContent _ -> None
    | _ -> failwith "This shouldn't happend, it's prevotes" in
  let found_set : MySet.t =
    count_prevotes ~prevote_selection:take_all_prevotes msg_log consensus_state
      global_state in
  let do_something () =
    Schedule
      (Clock.make prevote_timeout Prevote height round
         (on_timeout_prevote height round)) in
  if found_set = MySet.empty || consensus_state.step <> Prevote then
    None
  else
    Some (do_something ())

let lock_prevote_phase (_height : height) (_round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog global_state
    =
  let step = consensus_state.step in
  let left_set : MySet.t =
    select_proposal_process_round_matching_proposer msg_log consensus_state
      global_state in
  let right_set : MySet.t =
    count_prevotes msg_log consensus_state global_state in
  let extract_common_set (left_set : MySet.t) (right_set : MySet.t) : MySet.t =
    let filtered_left_set =
      MySet.filter (fun (v, _) -> is_valid global_state v) left_set in
    MySet.filter
      (fun (v, _) ->
        MySet.exists (fun (v', _) -> repr_of_value v = v') right_set)
      filtered_left_set in
  let do_something (consensus_state : consensus_state) (valid_tuples : MySet.t)
      =
    let value, _ = MySet.choose valid_tuples in
    let round = consensus_state.round in
    consensus_state.valid_round <- round;
    consensus_state.valid_value <- value;
    if consensus_state.step = Prevote then (
      consensus_state.locked_round <- round;
      consensus_state.locked_value <- value;
      consensus_state.step <- Precommit;
      Broadcast
        (PrecommitOP
           (consensus_state.height, consensus_state.round, repr_of_value value)))
    else
      DoNothing in
  let common_set = extract_common_set left_set right_set in
  if step = Proposal || common_set = MySet.empty then
    None
  else
    Some (do_something consensus_state common_set)

let shortcut_prevote_fails (_height : height) (_round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog global_state
    =
  let found_set = count_prevotes msg_log consensus_state global_state in
  let find_valid found_set : MySet.t =
    MySet.filter (fun (v, r) -> v = nil && r = consensus_state.round) found_set
  in
  let do_something consensus_state _found_set =
    consensus_state.step <- Precommit;
    Broadcast (PrecommitOP (consensus_state.height, consensus_state.round, nil))
  in
  let valid_set = find_valid found_set in
  if valid_set = MySet.empty || consensus_state.step <> Prevote then
    None
  else
    Some (do_something consensus_state valid_set)

let prepare_new_round_precommit_fail (height : height) (round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog global_state
    =
  let found_set =
    count_precommits msg_log consensus_state global_state
    |> MySet.filter (fun (_, r) -> r = consensus_state.round) in
  let do_something () =
    Schedule
      (Clock.make precommit_timeout Precommit height round
         (on_timeout_precommit height round)) in
  if found_set = MySet.empty then None else Some (do_something ())

let accept_block (_height : height) (process_round : round)
    (consensus_state : consensus_state) (msg_log : input_log) dlog global_state
    =
  let height = consensus_state.height in
  let extract_common_set (left_set : MySet.t) (right_set : MySet.t) =
    MySet.filter
      (fun (v, r) -> MySet.mem (repr_of_value v, r) right_set)
      left_set in
  let left_set =
    select_proposal_matching_several_rounds msg_log consensus_state global_state
  in
  let right_set = count_precommits msg_log consensus_state global_state in
  let valid_set = extract_common_set left_set right_set in
  let do_something (_consensus_state : consensus_state) (_common_set : MySet.t)
      =
    let value, _valid_round = MySet.choose valid_set in
    if is_valid global_state value then (
      OutputLog.set dlog height value process_round;
      (* Removing now irrelevant data from input_log *)
      prune msg_log height;
      Some (RestartTendermint (Int64.add height 1L, 0)))
    else
      Some DoNothing in
  let c =
    (* Should we NOT write in the output log *)
    match OutputLog.get dlog height with
    | Some (_, r) -> r >= process_round
    | None -> false in
  if valid_set = MySet.empty || c then
    None
  else
    do_something consensus_state valid_set

let handle_node_delay (height : height) (_round : round)
    (consensus_state : consensus_state) (msg_log : input_log) _dlog global_state
    =
  let actions =
    count_all_actions ~threshold_f:1. msg_log consensus_state global_state in
  let round = consensus_state.round in
  let apply_round_condition (actions : MySet.t) : MySet.t =
    MySet.filter (fun (_, r) -> r > round) actions in
  let do_something (_consensuus_state : consensus_state) (common_set : MySet.t)
      =
    let _, round = MySet.choose common_set in
    Some (RestartTendermint (height, round)) in
  let filtered_actions = apply_round_condition actions in
  if filtered_actions = MySet.empty then
    None
  else
    do_something consensus_state filtered_actions

let all_processes : process list =
  [
    start_round;
    response_to_proposal;
    precommit_failed_previous_round;
    lock_prevote_phase;
    shortcut_prevote_fails;
    prepare_default_precommit_prevote_phase;
    prepare_new_round_precommit_fail;
    accept_block;
    handle_node_delay;
    on_timeout_propose;
    on_timeout_prevote;
    on_timeout_precommit;
  ]
