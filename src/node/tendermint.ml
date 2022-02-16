open Helpers
open Tendermint_internals
open Tendermint_data
open Tendermint_processes
open Tendermint_helpers

module CI = Tendermint_internals

type t = {
  identity : State.identity;
  clocks : clock list IntSet.t;
  consensus_states : consensus_state IntSet.t;
  (* FIXME: we don't want this to be mutable *)
  mutable procs : (height * process) list;
  node_state : State.t;
  input_log : input_log;
  output_log : output_log;
}
(** Tendermint simplification: as Deku is not going to run several
    blockchain heights at the same time, we only consider one set of
    states and clocks. *)

type should_restart_tendermint =
  | DontRestart
  | RestartAtHeight of CI.height
  | RestartAtRound  of CI.round

let make identity node_state current_height =
  let clocks = IntSet.create 0 in
  let new_state = CI.fresh_state current_height in
  let states = IntSet.create 0 in
  IntSet.add states current_height new_state;
  let procs = List.map (fun x -> (current_height, x)) all_processes in
  let input_log = CD.empty () in
  let output_log = CD.OutputLog.empty () in
  {
    identity;
    clocks;
    consensus_states = states;
    node_state;
    procs;
    input_log;
    output_log;
  }

let current_height node =
  List.fold_left max 0L (IntSet.to_seq_keys node.consensus_states |> List.of_seq)

(* FIXME: bad design *)
let () =
  CI.produce_value :=
    fun state -> CI.block (Building_blocks.produce_block state)

(** Process messages in the queue and decide actions; pure function, to be interpeted in Lwt later.
    TODO: Ensures that messages received over network go through signature verification before adding them to input_log
    FIXME: we're not empyting the input_log atm *)
let tendermint_step node =
  let open Tendermint_processes in
  let rec exec_procs processes still_active network_actions =
    match processes with
    | ((height, process) as p) :: rest -> begin
      let consensus_state = IntSet.find node.consensus_states height in
      let round = consensus_state.CI.round in
      let message_log = node.input_log in
      let output = node.output_log in
      match
        process height round consensus_state message_log output node.node_state
      with
      (* The process precondition hasn't been activated *)
      | None -> exec_procs rest (p :: still_active) network_actions
      (* The process terminates with a network event *)
      | Some (Broadcast t) -> exec_procs rest still_active (t :: network_actions)
      (* The process terminates silently *)
      | Some DoNothing -> exec_procs rest still_active network_actions
      (* We accepted a block for the height *)
      | Some (RestartTendermint (height, 0)) ->
        (* Start new processes and forget about the older ones *)
        (* TODO: this is only valid for the (current) restricted version of Tendermint which does not
           support several cycles/heights running in parallel. *)
        ([], network_actions, RestartAtHeight height)
      | Some (RestartTendermint (_height, round)) ->
        ([], network_actions, RestartAtRound round)
      (* Add a new clock to the scheduler *)
      | Some (Schedule c) ->
        debug node.node_state
          ("Should start clock for step " ^ string_of_step c.Clock.step);
        let cs =
          IntSet.find_opt node.clocks height |> Option.value ~default:[] in
        IntSet.add node.clocks height (c :: cs);
        exec_procs rest still_active network_actions
    end
    | [] -> (still_active, network_actions, DontRestart) in
  let still_active, network_actions, should_restart =
    exec_procs node.procs [] [] in
  (* TODO: I don't want this to be mutable *)
  node.procs <- still_active;
  ({ node with procs = still_active }, List.rev network_actions, should_restart)
(* List.rev: Do we really need order on the network? *)

let add_to_input input_log height step content =
  let index = (height, step) in
  CD.add input_log index content

let is_valid_consensus_op state consensus_op =
  (* TODO: this is a filter for input log since it's only optimization (don't keep stuff from past)*)
  let open Result in
  let _all_operations_properly_signed = function
    | _ -> true in
  let h = CI.height consensus_op in
  let current_height = state.State.protocol.block_height in
  if current_height > h then
    let s =
      Printf.sprintf
        "new block has a lower height (%Ld) than the current state (%Ld)" h
        current_height in
    error s
  else
    ok ()

let broadcast_op state consensus_op =
  let node_address = state.State.identity.t in
  let node_state = state in
  let s1 = string_of_op consensus_op in
  let s2 = Crypto.Key_hash.to_string node_address in
  let hash = Crypto.BLAKE2B.hash (s1 ^ s2) in
  let signature =
    Protocol.Signature.sign ~key:node_state.State.identity.secret hash in
  Lwt.async (fun () ->
      let%await () = Lwt_unix.sleep 1.0 in
      match consensus_op with
      | PrecommitOP (_height, _round, Block b) ->
        let hash_signature =
          Protocol.Signature.sign ~key:node_state.State.identity.secret b.hash
        in
        Networking.broadcast_signature node_state
          {
            operation = consensus_op;
            sender = node_address;
            hash = b.hash;
            hash_signature;
            signature;
          }
      | _ ->
        Networking.broadcast_consensus_op node_state
          { operation = consensus_op; sender = node_address; signature })

let add_consensus_op node _update_state sender op =
  let input_log = node.input_log in
  let input_log =
    add_to_input input_log (CI.height op) (CI.step_of_op op)
      (CD.content_of_op sender op) in
  { node with input_log }
let rec exec_consensus node =
  let open CI in
  let node, network_actions, should_restart = tendermint_step node in

  (* Send all actions over the network *)
  List.iter (broadcast_op node.node_state) network_actions;
  match (node.procs, should_restart) with
  | _, RestartAtRound r ->
    let cur_height = current_height node in
    let cur_state = IntSet.find node.consensus_states cur_height in
    cur_state.round <- r;
    let new_processes = List.map (fun p -> (cur_height, p)) all_processes in
    exec_consensus { node with procs = new_processes }
  | [], RestartAtHeight new_height ->
    (* If we no longer have any active process, we start at  *)
    let new_state = CI.fresh_state new_height in
    IntSet.add node.consensus_states new_height new_state;
    let new_processes = List.map (fun p -> (new_height, p)) all_processes in
    exec_consensus { node with procs = new_processes }
  | _ ->
    (* Start the non-started clocks *)
    IntSet.map_inplace
      (fun height clocks -> List.map (start_clock height node) clocks)
      node.clocks;
    node

(* FIXME:? check for race conditions
   TODO: find a responsible adult to talk to about this.*)
and start_clock current_height node clock =
  let open Lwt in
  let open Tendermint_processes in
  if clock.Clock.started then
    clock
  else begin
    prerr_endline
      ("*** Starting clock for step " ^ string_of_step clock.Clock.step);
    async (fun () ->
        Lwt_unix.sleep (float_of_int clock.Clock.time) >>= fun () ->
        let input_log = node.input_log in
        let _ =
          add_to_input input_log current_height clock.Clock.step CD.Timeout
        in
        let new_node = exec_consensus node in
        (* FIXME: ? I don't want this to be mutable *)
        node.procs <- new_node.procs;
        Lwt.return_unit);
    { clock with started = true }
  end

let make_proposal height round block =
  CI.ProposalOP (height, round, CI.block block, -1)

let is_decided_on cstate (height : height) =
  let decision = cstate.output_log in
  match OutputLog.get decision height with
  | Some (Block b, round) -> Some (b, round)
  | _ -> None

(** Required to publish hash on Tezos *)
let previous_block cstate height =
  match OutputLog.get cstate.output_log (Int64.sub height 1L) with
  | Some (Block b, round) -> Some (b, round)
  | _ -> None

let height_from_op op = Tendermint_internals.height op

let round_from_op op = Tendermint_internals.round op

let get_block cstate height =
  match OutputLog.get cstate.output_log (Int64.sub height 1L) with
  | Some (Block b, _) -> b
  | _ -> failwith (Printf.sprintf "No block here %Ld" height)
