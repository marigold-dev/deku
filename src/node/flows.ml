open Helpers
open Crypto
open Protocol
open Domainslib
open Consensus
open Building_blocks
module Node = State

let write_state_to_file path protocol =
  let protocol_bin = Marshal.to_string protocol [] in
  Lwt.async (fun () ->
      Lwt_io.with_file ~mode:Output path (fun oc ->
          let%await () = Lwt_io.write oc protocol_bin in
          Lwt_io.flush oc))

let string_of_error = function
  | `Added_block_has_lower_block_height -> "Added block has lower block height"
  | `Added_block_not_signed_enough_to_desync ->
    "Added_block_not_signed_enough_to_desync"
  | `Added_signature_not_signed_enough_to_request ->
    "Added_signature_not_signed_enough_to_request"
  | `Already_known_block -> "Already_known_block"
  | `Already_known_signature -> "Already_known_signature"
  | `Block_not_signed_enough_to_apply -> "Block_not_signed_enough_to_apply"
  | `Failed_to_verify_payload -> "Failed to verify payload signature"
  | `Invalid_address_on_main_operation -> "Invalid_address_on_main_operation"
  | `Invalid_block string -> Format.sprintf "Invalid_block(%s)" string
  | `Invalid_block_when_applying -> "Invalid_block_when_applying"
  | `Invalid_nonce_signature -> "Invalid_nonce_signature"
  | `Invalid_signature_author -> "Invalid_signature_author"
  | `Invalid_signature_for_this_hash -> "Invalid_signature_for_this_hash"
  | `Invalid_state_root_hash -> "Invalid_state_root_hash"
  | `Not_current_block_producer -> "Not_current_block_producer"
  | `Not_a_json -> "Invalid json"
  | `Not_a_valid_request err -> Format.sprintf "Invalid request: %s" err
  | `Pending_blocks -> "Pending_blocks"
  | `Unknown_uri -> "Unknown_uri"
  | `Not_a_user_opertaion -> "Not_a_user_opertaion"
  | `Not_consensus_operation -> "Not_consensus_operation"
  | `Invalid_signature -> "Invalid_signature"
  | `Invalid_snapshot_height -> "Invalid_snapshot_height"
  | `Not_all_blocks_are_signed -> "Not_all_blocks_are_signed"
  | `State_root_not_the_expected -> "State_root_not_the_expected"
  | `Snapshots_with_invalid_hash -> "Snapshots_with_invalid_hash"
  | `Node_not_yet_initialized -> "Node_not_yet_initialized"
  | `Not_a_validator -> "Not_a_validator"
  | `Invalid_bootstrapper_signature -> "Invalid_bootstrapper_signal"

let print_error err = Log.error "%s" (string_of_error err)

type flag_node =
  [ `Invalid_block
  | `Invalid_signature ]

type ignore =
  [ `Added_block_not_signed_enough_to_desync
  | `Added_signature_not_signed_enough_to_request
  | `Already_known_block
  | `Already_known_signature
  | `Block_not_signed_enough_to_apply
  | `Not_current_block_producer
  | `Pending_blocks
  | `Added_block_has_lower_block_height ]

let reset_timeout = (ref (fun () -> assert false) : (unit -> unit) ref)

let get_state = (ref (fun () -> assert false) : (unit -> State.t) ref)

let set_state = (ref (fun _ -> assert false) : (State.t -> unit) ref)

let get_task_pool = (ref (fun () -> assert false) : (unit -> Task.pool) ref)

let received_block' = (ref (fun _ -> assert false) : (Block.t -> unit) ref)

let block_added_to_the_pool' =
  (ref (fun _ -> assert false)
    : (Node.t ->
      (Node.t -> Node.t) ->
      Block.t ->
      ( unit,
        [ `Added_block_not_signed_enough_to_desync
        | `Block_not_signed_enough_to_apply
        | `Invalid_block_when_applying
        | `Invalid_state_root_hash
        | `Not_current_block_producer
        | `Added_block_has_lower_block_height ] )
      result)
      ref)

let rec request_block_by_hash tries ~hash =
  if tries > 20 then raise Not_found;
  Lwt.catch
    (fun () ->
      let state = !get_state () in
      let validator_uri = find_random_validator_uri state in
      let%await block = Network.request_block_by_hash { hash } validator_uri in
      await (Option.get block))
    (fun _exn ->
      Printexc.print_backtrace stdout;
      request_block_by_hash (tries + 1) ~hash)

let request_block ~hash =
  Lwt.async (fun () ->
      let%await block = request_block_by_hash 0 ~hash in
      !received_block' block;
      Lwt.return ())

let rec request_protocol_snapshot tries =
  if tries > 20 then raise Not_found;
  Lwt.catch
    (fun () ->
      let state = !get_state () in
      let validator_uri = find_random_validator_uri state in
      Network.request_protocol_snapshot () validator_uri)
    (fun _exn ->
      Printexc.print_backtrace stdout;
      request_protocol_snapshot (tries + 1))

let () =
  Lwt.async_exception_hook :=
    fun exn ->
      Log.error "global_exception:  %a" Fmt.exn exn;
      Printexc.print_backtrace stderr

let pending = ref false

let load_snapshot snapshot_data =
  let open Network.Protocol_snapshot in
  let state = !get_state () in
  let%ok consensus =
    Consensus.load_snapshot ~snapshot:snapshot_data.snapshot
      ~additional_blocks:snapshot_data.additional_blocks
      ~last_block:snapshot_data.last_block
      ~last_block_signatures:snapshot_data.last_block_signatures state.consensus
  in
  let state = { state with consensus } in
  Ok (!set_state state)

let request_protocol_snapshot () =
  Lwt.async (fun () ->
      let%await snapshot = request_protocol_snapshot 0 in
      (match load_snapshot snapshot with
      | Ok _ -> ()
      | Error err -> print_error err);
      await ())

let request_previous_blocks state block =
  let consensus = state.Node.consensus in
  if
    block_matches_current_state_root_hash consensus block
    || block_matches_next_state_root_hash consensus block
  then
    request_block ~hash:block.Block.previous_hash
  else if not !pending then (
    pending := true;
    request_protocol_snapshot ())

let commit_state_hash state =
  Tezos_interop.Consensus.commit_state_hash state.Node.interop_context

let try_to_commit_state_hash ~prev_validators state block signatures =
  let open Node in
  let signatures_map =
    signatures
    |> Signatures.to_list
    |> List.map (fun signature ->
           let address = Signature.address signature in
           let key = Signature.public_key signature in
           let signature = Signature.signature signature in
           (address, (key, signature)))
    |> List.to_seq
    |> Address_map.of_seq in
  let validators =
    state.consensus.protocol.validators
    |> Validators.to_list
    |> List.map (fun validator -> validator.Validators.address) in
  let signatures =
    prev_validators
    |> Validators.to_list
    |> List.map (fun validator -> validator.Validators.address)
    |> List.map (fun address -> Address_map.find_opt address signatures_map)
  in
  Lwt.async (fun () ->
      let%await () =
        match state.config.identity.t = block.Block.author with
        | true -> Lwt.return_unit
        | false -> Lwt_unix.sleep 120.0 in
      commit_state_hash state ~block_height:block.block_height
        ~block_payload_hash:block.payload_hash
        ~withdrawal_handles_hash:block.withdrawal_handles_hash
        ~state_hash:block.state_root_hash ~validators ~signatures)

let applied_block state update_state ~prev_protocol ~block ~receipts
    ~self_signed ~snapshot_ref =
  (* If the [block.state_root_hash] is not equal to either the
     current state root hash or the next calculated state root hash, then
     the node has become out of sync with the chain. In this case we will not sign
     blocks, but will still apply blocks with enough signatures.
     TODO: we currently stay out of sync until the next state root hash update that
     we finish on time. But it would be good to be able to get back in sync
     as soon as we finish hashing. See https://github.com/marigold-dev/deku/pull/250 *)
  let is_new_state_root_hash =
    not
      (BLAKE2B.equal prev_protocol.state_root_hash block.Block.state_root_hash)
  in
  (match snapshot_ref with
  | Some snapshot_ref ->
    let task_pool = !get_task_pool () in
    let _task : unit Task.promise =
      Task.async task_pool (fun () ->
          let hash, data = Protocol.hash prev_protocol in
          Snapshots.set_snapshot_ref snapshot_ref { hash; data }) in
    ()
  | None -> ());
  write_state_to_file
    (state.Node.data_folder ^ "/state.bin")
    state.consensus.protocol;
  !reset_timeout ();
  if is_new_state_root_hash then (
    write_state_to_file
      (state.data_folder ^ "/prev_epoch_state.bin")
      prev_protocol;
    match self_signed with
    | Some signatures ->
      try_to_commit_state_hash ~prev_validators:prev_protocol.validators state
        block signatures
    | None -> ());
  let recent_operation_receipts =
    List.fold_left
      (fun results (hash, receipt) -> BLAKE2B.Map.add hash receipt results)
      state.recent_operation_receipts receipts in
  let applied_blocks =
    (Unix.time (), block) :: state.applied_blocks |> List.take 100 in
  update_state { state with recent_operation_receipts; applied_blocks }

let persist_trusted_membership_change state ~trusted_validator_membership_change
    =
  Lwt.async (fun () ->
      trusted_validator_membership_change
      |> Trusted_validators_membership_change.Set.elements
      |> state.Node.persist_trusted_membership_change)

let handle_consensus_effect effect =
  match effect with
  | Request_block { hash } -> request_block ~hash
  | Request_previous_blocks { block } ->
    request_previous_blocks (!get_state ()) block
  | Broadcast_block { block } -> broadcast_block (!get_state ()) ~block
  | Broadcast_signature { hash; signature } ->
    broadcast_signature (!get_state ()) ~hash ~signature
  | Broadcast_user_operation { user_operation } ->
    Lwt.async (fun () ->
        broadcast_user_operation_gossip (!get_state ()) { user_operation })
  | Applied_block { prev_protocol; block; receipts; self_signed; snapshot_ref }
    ->
    applied_block (!get_state ()) !set_state ~prev_protocol ~block ~receipts
      ~snapshot_ref ~self_signed
  | Persist_trusted_membership_change { trusted_validator_membership_change } ->
    persist_trusted_membership_change (!get_state ())
      ~trusted_validator_membership_change

let handle_consensus_operation operation =
  let state = !get_state () in
  let consensus, errors =
    operation
      (fun effect consensus ->
        let state = !get_state () in
        !set_state Node.{ state with consensus };
        handle_consensus_effect effect)
      state.consensus in

  List.iter
    (fun error ->
      match error with
      | #ignore -> ()
      | error -> Log.error "consensus_error:  %s" (string_of_error error))
    errors;
  let state = !get_state () in
  !set_state { state with consensus }

let received_block block =
  handle_consensus_operation (fun handler consensus ->
      Consensus.with_block handler block consensus)

let () = received_block' := received_block

let received_signature ~hash ~signature =
  handle_consensus_operation (fun handler consensus ->
      Consensus.with_signature handler ~hash ~signature consensus)

let parse_internal_tezos_transaction transaction =
  match transaction with
  | Tezos_interop.Consensus.Update_root_hash _ -> Error `Update_root_hash
  | Tezos_interop.Consensus.Deposit { ticket; amount; destination } ->
    let amount = Core_deku.Amount.of_int (Z.to_int amount) in
    Ok (Core_deku.Tezos_operation.Tezos_deposit { destination; amount; ticket })

let parse_internal_tezos_transactions tezos_internal_transactions =
  List.filter_map
    (fun transaction ->
      match parse_internal_tezos_transaction transaction with
      | Ok core_tezos_internal_transactions ->
        Some core_tezos_internal_transactions
      | Error `Update_root_hash -> None)
    tezos_internal_transactions

let append_operation operation =
  handle_consensus_operation (fun handler consensus ->
      Consensus.with_operation handler operation consensus)

let received_tezos_operation tezos_interop_operation =
  let open Protocol.Operation in
  let Tezos_interop.Consensus.{ hash; transactions } = tezos_interop_operation in
  let tezos_operation =
    Core_deku.Tezos_operation.make
      {
        tezos_operation_hash = hash;
        internal_operations = parse_internal_tezos_transactions transactions;
      } in
  let operation = Core_tezos tezos_operation in
  append_operation operation

let received_user_operation user_operation =
  append_operation (Core_user user_operation)

let received_consensus_operation state consensus_operation signature =
  let open Protocol.Operation in
  let%assert () =
    ( `Invalid_signature,
      Consensus.verify state.Node.config.identity.key signature
        consensus_operation ) in
  append_operation (Consensus consensus_operation);
  Ok ()

let find_block_by_hash state hash =
  Block_pool.find_block ~hash state.Node.consensus.block_pool

let find_block_level state = state.Node.consensus.protocol.block_height

let request_nonce state update_state uri =
  let nonce = Random.generate 32 |> Cstruct.to_string in
  let _state =
    update_state
      (let open Node in
      { state with uri_state = Node.Uri_map.add uri nonce state.uri_state })
  in
  BLAKE2B.hash nonce

let register_uri state update_state ~uri ~signature =
  let%ok nonce =
    Node.Uri_map.find_opt uri state.Node.uri_state
    |> Option.to_result ~none:`Unknown_uri in
  let%assert () =
    (`Invalid_nonce_signature, Signature.verify ~signature (BLAKE2B.hash nonce))
  in
  let _state =
    update_state
      {
        state with
        validators_uri =
          Node.Address_map.add
            (Signature.address signature)
            uri state.validators_uri;
      } in
  Ok ()

let request_withdraw_proof state ~hash =
  match state.Node.recent_operation_receipts |> BLAKE2B.Map.find_opt hash with
  | None -> Network.Withdraw_proof.Unknown_operation
  | Some (Receipt_contract_invocation _ | Receipt_contract_origination _) ->
    Network.Withdraw_proof.Operation_is_not_a_withdraw
  | Some (Receipt_tezos_withdraw withdrawal_handle) ->
    let last_block_hash = state.Node.consensus.protocol.last_block_hash in
    let withdrawal_handles_hash =
      match
        Block_pool.find_block ~hash:last_block_hash
          state.Node.consensus.block_pool
      with
      | None -> assert false
      | Some block -> block.Block.withdrawal_handles_hash in
    let proof =
      state.Node.consensus.protocol.core_state
      |> Core_deku.State.ledger
      |> Ledger.withdrawal_handles_find_proof withdrawal_handle in
    Ok { withdrawal_handles_hash; withdrawal_handle; proof }

let request_ticket_balance state ~ticket ~address =
  state.Node.consensus.protocol.core_state
  |> Core_deku.State.ledger
  |> Ledger.balance address ticket

let trusted_validators_membership ~payload ~signature =
  handle_consensus_operation (fun handler consensus ->
      Consensus.with_trusted_validators_membership_change handler ~payload
        ~signature consensus)

let request_in_sync state =
  let state = state.Node.consensus in
  Consensus.in_sync state

let handle_bootstrap_signal ~payload =
  handle_consensus_operation (fun handler consensus ->
      Consensus.with_bootstrap_signal handler ~payload consensus)
