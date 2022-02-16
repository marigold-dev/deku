(** Tendermint's consensus processes as describded at https://arxiv.org/pdf/1807.04938.pdf *)

module CI = Tendermint_internals
module CD = Tendermint_data

module Clock = struct
  type 'a t = {
    time : int;
    step : CI.consensus_step;
    on_timeout :
      CI.consensus_state -> CD.input_log -> CD.output_log -> State.t -> 'a;
    started : bool;
  }

  let make time step on_timeout = { time; step; on_timeout; started = false }
end

(* Values returned by processes, to be interpreted by the node as network
   CI.s *)
type consensus_action =
  | Broadcast         of CI.sidechain_consensus_op
  | Schedule          of consensus_action option Clock.t
  | RestartTendermint of (CI.height * CI.round)
  | DoNothing

type process =
  CI.height ->
  CI.round ->
  CI.consensus_state ->
  CD.input_log ->
  CD.output_log ->
  State.t ->
  consensus_action option
(** Tendermint processes *)

type clock = consensus_action option Clock.t
(** Clocks scheduled for timeouts, to be used in Tendermint *)

let on_timeout_propose (height : CI.height) (round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) _dlog
    _global_state =
  let do_something consensus_state =
    if
      height = consensus_state.CI.height
      && round = consensus_state.CI.round
      && consensus_state.CI.step = CI.Proposal
    then (
      consensus_state.CI.step <- CI.Prevote;
      Broadcast
        (CI.PrevoteOP
           (consensus_state.CI.height, consensus_state.CI.round, CI.nil)))
    else
      DoNothing in
  if CD.contains_timeout msg_log height CI.Proposal then
    Some (do_something consensus_state)
  else
    None

let on_timeout_prevote (height : CI.height) (round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) _dlog
    _global_state =
  let do_something consensus_state =
    if
      height = consensus_state.CI.height
      && round = consensus_state.CI.round
      && consensus_state.CI.step = CI.Prevote
    then (
      consensus_state.CI.step <- CI.Precommit;
      Broadcast
        (CI.PrecommitOP
           (consensus_state.CI.height, consensus_state.CI.round, CI.nil)))
    else
      DoNothing in
  if CD.contains_timeout msg_log height CI.Prevote then
    Some (do_something consensus_state)
  else
    None

let start_round (height : CI.height) (round : CI.round)
    (consensus_state : CI.consensus_state) (_msg_log : CD.input_log) _dlog
    global_state =
  consensus_state.CI.round <- round;
  consensus_state.CI.step <- Proposal;
  let return_action =
    if CI.i_am_proposer global_state height round then
      if consensus_state.CI.valid_value <> CI.nil then
        Some
          (Broadcast
             (CI.ProposalOP
                ( consensus_state.CI.height,
                  consensus_state.CI.round,
                  consensus_state.CI.valid_value,
                  consensus_state.CI.valid_round )))
      else
        Some
          (Broadcast
             (CI.ProposalOP
                ( consensus_state.CI.height,
                  consensus_state.CI.round,
                  !CI.produce_value global_state (* FIXME: *),
                  consensus_state.CI.valid_round )))
    else
      Some
        (Schedule
           (Clock.make CI.proposal_timeout CI.Proposal
              (on_timeout_propose height round))) in
  return_action

let on_timeout_precommit (height : CI.height) (round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) _dlog
    _global_state =
  let do_something consensus_state =
    if height = consensus_state.CI.height && round = consensus_state.CI.round
    then
      RestartTendermint (height, round + 1)
    else
      DoNothing in
  if CD.contains_timeout msg_log height CI.Precommit then
    Some (do_something consensus_state)
  else
    None

let response_to_proposal (height : CI.height) (round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) _dlog
    global_state =
  let _height = consensus_state.CI.height in
  let step = consensus_state.CI.step in
  let _round = consensus_state.CI.round in
  let do_something (consensus_state : CI.consensus_state)
      (found_set : CD.MySet.t) =
    let value, _ = CD.MySet.choose found_set in
    consensus_state.CI.step <- Prevote;
    if
      CI.is_valid global_state value
      && (consensus_state.locked_round = -1
         || consensus_state.locked_value = value)
    then
      Broadcast (CI.PrevoteOP (height, round, CI.repr_of_value value))
    else
      Broadcast (CI.PrevoteOP (height, round, CI.nil)) in
  let found_set =
    CD.select_proposal_valid_round_matching_proposer msg_log consensus_state
      global_state in
  let extract_valid_set (found_set : CD.MySet.t) =
    CD.MySet.filter (fun (_, r) -> r = -1) found_set in
  let valid_set = extract_valid_set found_set in
  if valid_set = CD.MySet.empty || step <> CI.Proposal then
    None
  else
    Some (do_something consensus_state found_set)

let precommit_failed_previous_round (_height : CI.height) (_round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) _dlog
    global_state =
  let height = consensus_state.CI.height in
  let step = consensus_state.CI.step in
  let round = consensus_state.CI.round in
  (* Returns list of (repr_value, round) with filtered round *)
  let extract_common_set (left_set : CD.MySet.t) (right_set : CD.MySet.t) :
      CD.MySet.t =
    let filtered_left_set =
      CD.MySet.filter (fun (_, r) -> r >= 0 && r < round) left_set in
    (* (v, r)*)
    let filtered_right_set =
      CD.MySet.filter (fun (_, r) -> r >= 0 && r < round) right_set in
    (* (rv, r)*)
    CD.MySet.filter
      (fun (v, r) -> CD.MySet.mem (CI.repr_of_value v, r) filtered_right_set)
      filtered_left_set in
  let do_something (consensus_state : CI.consensus_state)
      (common_set : CD.MySet.t) =
    let value, valid_round = CD.MySet.choose common_set in
    consensus_state.CI.step <- Prevote;
    if
      CI.is_valid global_state value
      && (consensus_state.locked_round <= valid_round
         || consensus_state.locked_value = value)
    then
      Broadcast (CI.PrevoteOP (height, round, CI.repr_of_value value))
    else
      Broadcast (CI.PrevoteOP (height, round, CI.nil)) in
  let left_set =
    CD.select_proposal_valid_round_matching_proposer msg_log consensus_state
      global_state in
  let right_set = CD.count_prevotes msg_log consensus_state global_state in
  let common_set = extract_common_set left_set right_set in
  if
    left_set = CD.MySet.empty
    || right_set = CD.MySet.empty
    || step <> CI.Proposal
    || common_set = CD.MySet.empty
  then
    None
  else
    Some (do_something consensus_state common_set)

let prepare_default_precommit_prevote_phase (height : CI.height)
    (round : CI.round) (consensus_state : CI.consensus_state)
    (msg_log : CD.input_log) _dlog global_state =
  let take_all_prevotes =
    let open Tendermint_data in
    function
    | PrevoteContent content when content.process_round = consensus_state.round
      ->
      Some
        ( (CI.nil, content.process_round),
          CI.get_weight global_state content.sender )
    | PrevoteContent _ -> None
    | _ -> failwith "This shouldn't happend, it's prevotes" in
  let found_set : CD.MySet.t =
    CD.count_prevotes ~prevote_selection:take_all_prevotes msg_log
      consensus_state global_state in
  let do_something () =
    Schedule
      (Clock.make CI.prevote_timeout CI.Prevote
         (on_timeout_prevote height round)) in
  if found_set = CD.MySet.empty || consensus_state.CI.step <> CI.Prevote then
    None
  else
    Some (do_something ())

let lock_prevote_phase (_height : CI.height) (_round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) _dlog
    global_state =
  let step = consensus_state.CI.step in
  let left_set : CD.MySet.t =
    CD.select_proposal_process_round_matching_proposer msg_log consensus_state
      global_state in
  let right_set : CD.MySet.t =
    CD.count_prevotes msg_log consensus_state global_state in
  let extract_common_set (left_set : CD.MySet.t) (right_set : CD.MySet.t) :
      CD.MySet.t =
    let filtered_left_set =
      CD.MySet.filter (fun (v, _) -> CI.is_valid global_state v) left_set in
    CD.MySet.filter
      (fun (v, _) ->
        CD.MySet.exists (fun (v', _) -> CI.repr_of_value v = v') right_set)
      filtered_left_set in
  let do_something (consensus_state : CI.consensus_state)
      (valid_tuples : CD.MySet.t) =
    let value, _ = CD.MySet.choose valid_tuples in
    let round = consensus_state.CI.round in
    consensus_state.CI.valid_round <- round;
    consensus_state.CI.valid_value <- value;
    if consensus_state.CI.step = CI.Prevote then (
      consensus_state.locked_round <- round;
      consensus_state.locked_value <- value;
      Broadcast
        (CI.PrecommitOP
           ( consensus_state.CI.height,
             consensus_state.CI.round,
             CI.repr_of_value value )))
    else
      DoNothing in
  let common_set = extract_common_set left_set right_set in
  if step = CI.Proposal || common_set = CD.MySet.empty then
    None
  else
    Some (do_something consensus_state common_set)

let shortcut_prevote_fails (_height : CI.height) (_round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) _dlog
    global_state =
  let found_set = CD.count_prevotes msg_log consensus_state global_state in
  let find_valid found_set : CD.MySet.t =
    CD.MySet.filter
      (fun (v, r) -> v = CI.nil && r = consensus_state.CI.round)
      found_set in
  let do_something consensus_state _found_set =
    consensus_state.CI.step <- CI.Precommit;
    Broadcast
      (CI.PrecommitOP
         (consensus_state.CI.height, consensus_state.CI.round, CI.nil)) in
  let valid_set = find_valid found_set in
  if valid_set = CD.MySet.empty || consensus_state.CI.step <> CI.Prevote then
    None
  else
    Some (do_something consensus_state valid_set)

let prepare_new_round_precommit_fail (height : CI.height) (round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) _dlog
    global_state =
  let found_set =
    CD.count_precommits msg_log consensus_state global_state
    |> CD.MySet.filter (fun (_, r) -> r = consensus_state.CI.round) in
  let do_something () =
    Schedule
      (Clock.make CI.precommit_timeout CI.Precommit
         (on_timeout_precommit height round)) in
  if found_set = CD.MySet.empty then None else Some (do_something ())

let accept_block (_height : CI.height) (round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) dlog
    global_state =
  let height = consensus_state.CI.height in
  let extract_common_set (left_set : CD.MySet.t) (right_set : CD.MySet.t) =
    CD.MySet.filter
      (fun (v, r) -> CD.MySet.mem (CI.repr_of_value v, r) right_set)
      left_set in
  let left_set =
    CD.select_proposal_matching_several_rounds msg_log consensus_state
      global_state in
  let right_set = CD.count_precommits msg_log consensus_state global_state in
  let valid_set = extract_common_set left_set right_set in
  let do_something (_consensus_state : CI.consensus_state)
      (_common_set : CD.MySet.t) =
    let value, _valid_round = CD.MySet.choose valid_set in
    if CI.is_valid global_state value then (
      CD.OutputLog.set dlog height value round;
      (* Removing now irrelevant data from input_log *)
      CD.prune msg_log height;
      Some (RestartTendermint (Int64.add height 1L, 0)))
    else
      Some DoNothing in
  let c =
    (* Should we NOT write in the output log *)
    match CD.OutputLog.get dlog height with
    | Some (_, r) -> r >= round
    | None -> false in
  if valid_set = CD.MySet.empty || c then
    None
  else
    do_something consensus_state valid_set

let handle_node_delay (height : CI.height) (_round : CI.round)
    (consensus_state : CI.consensus_state) (msg_log : CD.input_log) dlog
    global_state =
  let actions = CD.count_all_actions msg_log consensus_state global_state in
  let round = consensus_state.CI.round in
  let apply_round_condition (actions : CD.MySet.t) : CD.MySet.t =
    CD.MySet.filter (fun (_, r) -> r > consensus_state.CI.round) actions in
  let do_something (consensus_state : CI.consensus_state) (round : CI.round) =
    start_round height round consensus_state msg_log dlog global_state in
  let filtered_actions = apply_round_condition actions in
  if filtered_actions = CD.MySet.empty then
    None
  else
    do_something consensus_state round

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
