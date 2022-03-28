open Helpers
open Tendermint_internals
open Tendermint_data
open Tendermint_processes
open Tendermint_helpers

type t = {
  identity : State.identity;
  clocks : clock list IntSet.t;
  consensus_states : consensus_state IntSet.t;
  mutable procs : (height * process) list;
  node_state : State.t;
  input_log : input_log;
  output_log : OutputLog.t;
}

type should_restart_tendermint =
  | DontRestart
  | RestartAtHeight of height
  | RestartAtRound  of round

let make node_state current_height =
  let identity = node_state.State.identity in
  let clocks = IntSet.create 0 in
  let new_state = fresh_state current_height in
  let states = IntSet.create 0 in
  IntSet.add states current_height new_state;
  let procs = List.map (fun x -> (current_height, x)) all_processes in
  let input_log = empty () in
  let output_log = OutputLog.empty () in
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
  produce_value := fun state -> block (Building_blocks.produce_block state)

(** Process messages in the queue and decide actions; pure function, to be interpeted in Lwt later.
    TODO: Ensures that messages received over network go through signature verification before adding them to input_log
    FIXME: we're not empyting the input_log atm *)
let tendermint_step node =
  let open Tendermint_processes in
  let rec exec_procs processes still_active network_actions =
    match processes with
    | ((height, process) as p) :: rest -> begin
      let consensus_state = IntSet.find node.consensus_states height in
      let round = consensus_state.Tendermint_internals.round in
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
        ([], [], RestartAtHeight height)
      | Some (RestartTendermint (_height, round)) ->
        ([], network_actions, RestartAtRound round)
      (* Add a new clock to the scheduler *)
      | Some (Schedule c) ->
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
  add input_log index content

let is_valid_consensus_op state consensus_op =
  (* TODO: ConsensusStep2 this is a filter for input log since it's only optimization (don't keep stuff from past)*)
  let open Result in
  let _all_operations_properly_signed = function
    | _ -> true in
  let h = height consensus_op in
  let current_height = state.State.protocol.block_height in
  if current_height > h then
    let s =
      Printf.sprintf
        "new block has a lower height (%Ld) than the current state (%Ld)" h
        current_height in
    error s
  else
    ok ()

(** Hashes stuff and broadcast the corresponding consensus operation *)
let broadcast_op state consensus_op =
  (* We need two hashes: one corresponding to the block, height and round, which will ultimately get
     posted on Tezos; and one of the consensus operation *)
  let node_address = state.State.identity.t in
  let secret_key = state.State.identity.secret in
  (* Hashing the operation *)
  let operation_hash = hash_of_consensus_op consensus_op node_address in
  let operation_signature =
    Protocol.Signature.sign ~key:secret_key operation_hash in
  (* Hash the value+height+round even if it's nil *)
  let block_hash = hash_of_consensus_value consensus_op in
  (* DEBUGGING: this can also be useful:
     debug state (Printf.sprintf "sending %s" (string_of_op consensus_op)); *)
  let block_signature = Protocol.Signature.sign ~key:secret_key block_hash in
  Lwt.async (fun () ->
      let%await () = Lwt_unix.sleep 0.3 in
      Networking.broadcast_consensus_op state
        {
          operation = consensus_op;
          sender = node_address;
          hash = block_hash;
          block_signature;
          operation_signature;
        })

let add_consensus_op node sender op =
  let input_log = node.input_log in
  let input_log =
    add_to_input input_log (height op) (step_of_op op) (content_of_op sender op)
  in
  Lwt.return { node with input_log }

let rec exec_consensus node =
  (* debug node.node_state
     (Printf.sprintf "State is currently at height %Ld"
     (node.node_state.State.protocol.Protocol.block_height)); *)
  (* debug node.node_state (Printf.sprintf "length of procs: %d" (List.length node.procs));*)
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
    let new_state = fresh_state new_height in
    IntSet.add node.consensus_states new_height new_state;
    let new_processes = List.map (fun p -> (new_height, p)) all_processes in
    { node with procs = new_processes }
    (* exec_consensus { node with procs = new_processes } *)
  | _ ->
    (* Start the non-started clocks *)
    IntSet.map_inplace
      (fun _height clocks ->
        List.map
          (fun c -> start_clock node c.Clock.height c.Clock.round c)
          clocks)
      node.clocks;
    node

(* TODO: don't use async and global state,
   maintain an internal list of clocks started at a given time and regularly call exec_consensus
   from flows again *)

and start_clock node clock_height clock_round clock =
  let open Lwt in
  let open Tendermint_processes in
  if clock.Clock.started then
    clock
  else begin
    async (fun () ->
        Lwt_unix.sleep 2.
        (* FIXME (float_of_int clock.Clock.time) *) >>= fun () ->
        (* Checks that the clock is still relevant *)
        let current_height = current_height node in
        (* FIXME: fragile code; we should fix the whole way we handle clocks with Lwt *)
        let current_round =
          (IntSet.find node.consensus_states current_height)
            .Tendermint_internals.round in
        if current_height > clock_height || current_round > clock_round then
          Lwt.return_unit
        else
          let input_log = node.input_log in
          let _ = add_to_input input_log clock_height clock.Clock.step Timeout in
          let new_node = exec_consensus node in
          (* FIXME: ? I don't want this to be mutable *)
          node.procs <- new_node.procs;
          Lwt.return_unit);
    { clock with started = true }
  end

let make_proposal height round b = ProposalOP (height, round, block b, -1)

let get_block_opt cstate height =
  match OutputLog.get cstate.output_log height with
  | Some (Block b, round) -> Some (b, round)
  | _ -> None
