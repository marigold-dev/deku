open Helpers
open Prenode.Helpers
open Crypto
open Protocol
module Node = State
let is_valid_block state block =
  let is_all_operations_properly_signed _block = true in
  let%assert () =
    ( Printf.sprintf
        "new block has a lower block height (%Ld) than the current state (%Ld)"
        block.Block.block_height state.Node.protocol.block_height,
      block.Block.block_height >= state.Node.protocol.block_height ) in
  let%assert () =
    ( "some operation in the block is not properly signed",
      is_all_operations_properly_signed block ) in
  Ok ()
let is_next state block = Protocol.is_next state.Node.protocol block
let has_next_block_to_apply state ~hash =
  Block_pool.find_next_block_to_apply ~hash state.Node.block_pool
  |> Option.is_some
let is_known_block state ~hash =
  Option.is_some (Block_pool.find_block state.Node.block_pool ~hash)
let is_known_signature state ~hash ~signature =
  let%default () = false in
  let%some signatures = Block_pool.find_signatures ~hash state.Node.block_pool in
  Some (Signatures.mem signature signatures)
let is_signed_by_self state ~hash =
  let%default () = false in
  let%some signatures = Block_pool.find_signatures ~hash state.Node.block_pool in
  Some (Signatures.is_self_signed signatures)
let is_current_producer state ~key_hash =
  let%default () = false in
  let%some current_producer = get_current_block_producer state.Node.protocol in
  Some (current_producer.Validator_internals.Validators.address = key_hash)
let minimum_signable_time_between_epochs = 10.0
let maximum_signable_time_between_epochs = 20.0

(** Used to add a delay between a tezos operation being confirmed,
  needs to be bigger than the polling interval for operations *)
let minimum_waiting_period_for_tezos_operation = 5.0
(* TODO: this is an workaround solution, replace it by Tezos_rpc *)

let block_matches_current_state_root_hash state block =
  BLAKE2B.equal block.Block.state_root_hash state.Node.protocol.state_root_hash
let block_matches_next_state_root_hash state block =
  let%default () = false in
  let%some { hash = next_state_root_hash; _ } =
    Snapshots.get_next_snapshot state.Node.snapshots in
  Some (BLAKE2B.equal block.Block.state_root_hash next_state_root_hash)
let block_has_signable_state_root_hash ~current_time state block =
  let time_since_last_epoch =
    current_time -. state.Node.protocol.last_state_root_update in
  if block_matches_current_state_root_hash state block then
    time_since_last_epoch <= maximum_signable_time_between_epochs
  else
    block_matches_next_state_root_hash state block
    && time_since_last_epoch >= minimum_signable_time_between_epochs

let is_signable_validators_ops : State.t -> Message.t -> bool =
  state block
  =
  (* NEW VERSION HERE *)
  let state =
    Prenode.Validators_Prenode.update_membership state.validators_prenode in
  let last_seen_membership_change_timestamp =
    Prenode.Validators_Prenode.get_last_change_timestamp state in

  let current_time = Unix.time () in
  let next_allowed_membership_change_timestamp =
    last_seen_membership_change_timestamp +. (24. *. 60. *. 60.) in

  let is_trusted_protocol_operation : State.t -> Block.t -> bool =
   fun state block ->
    (* TODO: HERE craft a message for Prenode.Validators with block as payload *)
    let msg = Prenode.Message.empty in
    Prenode.Validators_Prenode.is_validator_already_registered msg
      state.validators_prenode in

  current_time > next_allowed_membership_change_timestamp
  && Prenode.Validators_Prenode.is_validator_already_registered msg state

let is_signable_msgs : State.t -> Message.t list -> bool =
 fun state msgs ->
  let validators_msgs = Prenode.Validators_Prenode.filter_msgs msgs in
  let validators_results =
    List.map (is_signable_validators_ops state) validators_msgs in

  List.for_all (fun x -> True) validators_results

let is_signable state block =
  (* WE ARE HERE WE NEED UPDATES*)
  let state =
    Prenode.Validators_Prenode.update_membership state.validators_prenode in

  let is_trusted_operation operation =
    match operation with
    | Protocol.Operation.Core_tezos _ ->
      Node.Operation_map.mem operation state.pending_operations
    | Core_user _ -> true
    | Consensus consensus_operation ->
      failwith "Consensus operation do not exist anymore" in
  let all_operations_are_trusted =
    List.for_all is_trusted_operation block.Block.operations in
  is_next state block
  && (not (is_signed_by_self state ~hash:block.hash))
  && is_current_producer state ~key_hash:block.author
  && (not (has_next_block_to_apply state ~hash:block.hash))
  && all_operations_are_trusted
  && block_has_signable_state_root_hash ~current_time state block
let sign ~key block = Block.sign ~key block

let should_start_new_epoch last_state_root_update current_time =
  let avoid_jitter = 1.0 in
  current_time -. last_state_root_update -. avoid_jitter
  >= minimum_signable_time_between_epochs
  [@@ocaml.doc
    " Calculates whether to start sending a new state root hash.\n\n\
    \    The state root epoch is the interval (in blocks) between state root\n\
    \    hash updates. Thus, a new epoch is triggered by applying a block with\n\
    \    a new state root hash. The block producer decides when to send\n\
    \    blocks with new state root hashes. To enforce that he does so on time,\n\
    \    validators reject blocks with updates that occur to soon or too late\n\
    \    (see [Protocol.apply]).\n\n\
    \    The block producer uses this function to determine when to send a\n\
    \    block with an updated state root hash.\n"]

(** Can only included a tezos operation if enough time has already elapsed *)
let can_include_tezos_operation ~current_time ~requested_at =
  current_time -. requested_at > minimum_waiting_period_for_tezos_operation

let produce_block state =
  let current_time = Unix.time () in
  let start_new_epoch =
    should_start_new_epoch state.Node.protocol.last_state_root_update
      current_time in
  let next_state_root_hash =
    if start_new_epoch then
      let%some snapshot = Snapshots.get_next_snapshot state.snapshots in
      Some snapshot.hash
    else
      None in
  let operations =
    (* TODO: fold into list on Helpers *)
    Node.Operation_map.fold
      (fun operation requested_at operations ->
        match operation with
        | Operation.Core_tezos _ ->
          if can_include_tezos_operation ~current_time ~requested_at then
            operation :: operations
          else
            operations
        | Core_user _
        | Consensus _ ->
          operation :: operations)
      state.pending_operations [] in
  Block.produce ~state:state.Node.protocol ~author:state.identity.t
    ~next_state_root_hash ~operations
let is_valid_block_height state block_height =
  block_height >= 1L && block_height <= state.Node.protocol.block_height
let signatures_required state =
  let number_of_validators =
    Prenode.Validators_Prenode.get_number_of_validators state.validators_prenode
  in

  let open Float in
  to_int (ceil (of_int number_of_validators *. (2.0 /. 3.0)))
let append_signature state update_state ~hash ~signature =
  let block_pool =
    Block_pool.append_signature
      ~signatures_required:(signatures_required state)
      ~hash signature state.Node.block_pool in
  update_state { state with block_pool }
let add_block_to_pool state update_state block =
  let block_pool = Block_pool.append_block block state.Node.block_pool in
  update_state { state with block_pool }
let apply_block state update_state block =
  let%ok state = Node.apply_block state block in
  Ok (update_state state)

(*

TODO: HERE

let _ops_from_msgs : Prenode.Message.t list -> Validator_internals.Action.t list =
     fun msgs ->
       let ops =

   let _clean_operations : State.t -> Message.t list -> State.t * Message.t list =
     fun state msgs ->
       (* Remove ops that match Validators *)
       state, msgs


   let clean_msgs : State.t -> (State.t -> State.t) -> Message.t list -> State.t =
     fun state update_state msgs ->
       (* Remove operations included in Block from pending operation *)
       (* TODO: FIXME: call ops prenode to remove ops included in msgs from pendings ops *)
       (* First, only _clean_operations  *)
       state
*)

let clean state update_state block =
  (* TODO: HERE *)
  let pending_operations =
    List.fold_left
      (fun pending_operations operation ->
        Node.Operation_map.remove operation pending_operations)
      state.State.pending_operations block.Block.operations in
  let trusted_validator_membership_change =
    List.fold_left
      (fun trusted_validator_membership_change operation ->
        match operation with
        | Operation.Core_tezos _
        | Core_user _ ->
          trusted_validator_membership_change
        | Consensus action ->
          Prenode.Validators_Prenode.update_trusted action
            state.validators_prenode)
      (Prenode.Validators_Prenode.get_membership state.validators_prenode)
      block.operations in

  let validators_prenode =
    Prenode.Validators_Prenode.set_trusted_change
      trusted_validator_membership_change state.validators_prenode in

  let trusted_set =
    Prenode.Validators_Prenode.get_trusted_change_opt validators_prenode in
  let trusted_set =
    match trusted_set with
    | None -> failwith "WHAT"
    | Some trusted_set -> trusted_set in

  Lwt.async (fun () ->
      trusted_validator_membership_change
      |> trusted_set.elements
      |> t.persist_trusted_change);
  (* TODO: this should be in Prenode validators *)
  update_state { state with validators_prenode; pending_operations }

let find_random_validator_uri state =
  let random_int v = v |> Int32.of_int |> Random.int32 |> Int32.to_int in
  let validators =
    Prenode.Validators_Prenode.get_validators_list state.validators_prenode
  in
  let rec safe_validator_uri () =
    let validator = List.nth validators (random_int (List.length validators)) in
    if state.Node.identity.t = validator.Validator_internals.Validators.address
    then
      safe_validator_uri ()
    else
      match
        Node.Address_map.find_opt
          validator.Validator_internals.Validators.address state.validators_uri
      with
      | Some uri -> uri
      | None -> safe_validator_uri () in
  safe_validator_uri ()

let validator_uris state =
  let validators_addresses =
    Prenode.Validators_Prenode.get_validators_address_list
      state.validators_prenode in
  List.filter_map
    (fun address -> Node.Address_map.find_opt address state.Node.validators_uri)
    validators_addresses

let broadcast_signature state ~hash ~signature =
  let uris = validator_uris state in
  Lwt.async (fun () -> Network.broadcast_signature uris { hash; signature })
let broadcast_block_and_signature state ~block ~signature =
  let uris = validator_uris state in
  Lwt.async (fun () ->
      let%await () = Lwt_unix.sleep 1.0 in
      Network.broadcast_block_and_signature uris { block; signature })
let broadcast_user_operation_gossip state operation =
  let uris = validator_uris state in
  Network.broadcast_user_operation_gossip uris operation
