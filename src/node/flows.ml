open Helpers
open Crypto
open Protocol
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

let print_error err =
  Format.eprintf "\027[31mError: %s\027[m\n%!" (string_of_error err)

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
let received_block' =
  (ref (fun _ -> assert false)
    : (Node.t ->
      (Node.t -> Node.t) ->
      Block.t ->
      ( unit,
        [ `Added_block_not_signed_enough_to_desync
        | `Already_known_block
        | `Block_already_in_the_pool
        | `Block_not_signed_enough_to_apply
        | `Invalid_block                           of string
        | `Invalid_block_when_applying
        | `Invalid_state_root_hash
        | `Not_current_block_producer
        | `Pending_blocks
        | `Added_block_has_lower_block_height ] )
      result)
      ref)
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
      let%await block =
        Networking.request_block_by_hash { hash } validator_uri in
      await (Option.get block))
    (fun _exn ->
      Printexc.print_backtrace stdout;
      request_block_by_hash (tries + 1) ~hash)
let request_block ~hash =
  Lwt.async (fun () ->
      let%await block = request_block_by_hash 0 ~hash in
      let state = !get_state () in
      match
        !received_block' state
          (fun state ->
            !set_state state;
            state)
          block
      with
      | Ok () -> await ()
      | Error _err -> await ())
let rec request_protocol_snapshot tries =
  if tries > 20 then raise Not_found;
  Lwt.catch
    (fun () ->
      let state = !get_state () in
      let validator_uri = find_random_validator_uri state in
      Networking.request_protocol_snapshot () validator_uri)
    (fun _exn ->
      Printexc.print_backtrace stdout;
      request_protocol_snapshot (tries + 1))

let () =
  Lwt.async_exception_hook :=
    fun exn ->
      Printexc.to_string exn |> Format.eprintf "global_exception: %s\n%!";
      Printexc.print_backtrace stderr
let pending = ref false
let load_snapshot snapshot_data =
  let open Networking.Protocol_snapshot in
  let%ok state =
    Node.load_snapshot ~snapshot:snapshot_data.snapshot
      ~additional_blocks:snapshot_data.additional_blocks
      ~last_block:snapshot_data.last_block
      ~last_block_signatures:snapshot_data.last_block_signatures (!get_state ())
  in
  Ok (!set_state state)
let request_protocol_snapshot () =
  Lwt.async (fun () ->
      let%await snapshot = request_protocol_snapshot 0 in
      (match load_snapshot snapshot with
      | Ok _ -> ()
      | Error err -> print_error err);
      await ())
let request_previous_blocks state block =
  if
    block_matches_current_state_root_hash state block
    || block_matches_next_state_root_hash state block
  then
    request_block ~hash:block.Block.previous_hash
  else if not !pending then (
    pending := true;
    request_protocol_snapshot ())
let try_to_produce_block state update_state =
  let%assert () =
    ( `Not_current_block_producer,
      is_current_producer state ~key_hash:state.identity.t ) in
  let block = produce_block state in
  let signature = sign ~key:state.identity.secret block in
  let state = append_signature state update_state ~signature ~hash:block.hash in
  broadcast_block_and_signature state ~block ~signature;
  Ok ()
let try_to_sign_block state update_state block =
  if is_signable state block then (
    let signature = sign ~key:state.identity.secret block in
    broadcast_signature state ~hash:block.hash ~signature;
    append_signature state update_state ~hash:block.hash ~signature)
  else
    state
let commit_state_hash state =
  Tezos_interop.Consensus.commit_state_hash ~context:state.Node.interop_context
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
    state.protocol.validators
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
        match state.identity.t = block.Block.author with
        | true -> Lwt.return_unit
        | false -> Lwt_unix.sleep 120.0 in
      commit_state_hash state ~block_height:block.block_height
        ~block_payload_hash:block.payload_hash
        ~withdrawal_handles_hash:block.withdrawal_handles_hash
        ~state_hash:block.state_root_hash ~validators ~signatures)
let rec try_to_apply_block state update_state block =
  let%assert () =
    ( `Block_not_signed_enough_to_apply,
      Block_pool.is_signed ~hash:block.Block.hash state.Node.block_pool ) in
  let%assert () =
    ( `Invalid_state_root_hash,
      block_matches_current_state_root_hash state block
      || block_matches_next_state_root_hash state block ) in
  let prev_protocol = state.protocol in
  let is_new_state_root_hash =
    not (BLAKE2B.equal state.protocol.state_root_hash block.state_root_hash)
  in
  let%ok state = apply_block state update_state block in
  write_state_to_file (state.Node.data_folder ^ "/state.bin") state.protocol;
  !reset_timeout ();
  let state = clean state update_state block in
  if is_new_state_root_hash then (
    write_state_to_file
      (state.data_folder ^ "/prev_epoch_state.bin")
      prev_protocol;
    match Block_pool.find_signatures ~hash:block.hash state.block_pool with
    | Some signatures when Signatures.is_self_signed signatures ->
      try_to_commit_state_hash ~prev_validators:prev_protocol.validators state
        block signatures
    | _ -> ());
  match
    Block_pool.find_next_block_to_apply ~hash:block.Block.hash state.block_pool
  with
  | Some block ->
    let state = try_to_sign_block state update_state block in
    try_to_apply_block state update_state block
  | None -> try_to_produce_block state update_state

and block_added_to_the_pool state update_state block =
  let state =
    match
      Block_pool.find_signatures ~hash:block.Block.hash state.Node.block_pool
    with
    | Some signatures when Signatures.is_signed signatures ->
      let snapshots =
        Snapshots.append_block ~pool:state.Node.block_pool (block, signatures)
          state.snapshots in
      { state with snapshots }
    | Some _signatures -> state
    | None -> state in
  if is_next state block then
    let state = try_to_sign_block state update_state block in
    try_to_apply_block state update_state block
  else
    let%assert () =
      ( `Added_block_not_signed_enough_to_desync,
        Block_pool.is_signed ~hash:block.hash state.block_pool ) in
    let%assert () =
      ( `Added_block_has_lower_block_height,
        block.block_height > state.protocol.block_height ) in
    match Block_pool.find_block ~hash:block.previous_hash state.block_pool with
    | Some block -> block_added_to_the_pool state update_state block
    | None ->
      request_previous_blocks state block;
      Ok ()
let () = block_added_to_the_pool' := block_added_to_the_pool
let received_block state update_state block =
  let%ok () =
    is_valid_block state block
    |> Result.map_error (fun msg -> `Invalid_block msg) in
  let%assert () =
    (`Already_known_block, not (is_known_block state ~hash:block.Block.hash))
  in
  let state = add_block_to_pool state update_state block in
  block_added_to_the_pool state update_state block
let () = received_block' := received_block
let received_signature state update_state ~hash ~signature =
  let%assert () =
    (`Invalid_signature_for_this_hash, Signature.verify ~signature hash) in
  let%assert () =
    (`Already_known_signature, not (is_known_signature state ~hash ~signature))
  in
  let state = append_signature state update_state ~hash ~signature in
  let%assert () =
    ( `Added_signature_not_signed_enough_to_request,
      Block_pool.is_signed ~hash state.Node.block_pool ) in
  match Block_pool.find_block ~hash state.Node.block_pool with
  | Some block -> block_added_to_the_pool state update_state block
  | None ->
    request_block ~hash;
    Ok ()
let parse_internal_tezos_transaction transaction =
  match transaction with
  | Tezos_interop.Consensus.Update_root_hash _ -> Error `Update_root_hash
  | Tezos_interop.Consensus.Deposit { ticket; amount; destination } ->
    let amount = Core.Amount.of_int (Z.to_int amount) in
    Ok (Core.Tezos_operation.Tezos_deposit { destination; amount; ticket })
let parse_internal_tezos_transactions tezos_internal_transactions =
  List.filter_map
    (fun transaction ->
      match parse_internal_tezos_transaction transaction with
      | Ok core_tezos_internal_transactions ->
        Some core_tezos_internal_transactions
      | Error `Update_root_hash -> None)
    tezos_internal_transactions
let received_tezos_operation state update_state tezos_interop_operation =
  let open Protocol.Operation in
  let Tezos_interop.Consensus.{ hash; transactions } = tezos_interop_operation in
  let tezos_operation =
    Core.Tezos_operation.make
      {
        tezos_operation_hash = hash;
        internal_operations = parse_internal_tezos_transactions transactions;
      } in
  let operation = Core_tezos tezos_operation in
  let (_ : State.t) =
    update_state
      (let open Node in
      { state with pending_operations = operation :: state.pending_operations })
  in
  ()
let received_user_operation state update_state user_operation =
  let open Protocol.Operation in
  let operation = Core_user user_operation in
  let operation_exists =
    List.exists (fun op -> equal op operation) state.Node.pending_operations
  in
  if not operation_exists then (
    Lwt.async (fun () ->
        Networking.broadcast_user_operation_gossip state { user_operation });
    let (_ : State.t) =
      update_state
        (let open Node in
        {
          state with
          pending_operations = operation :: state.pending_operations;
        }) in
    ());
  Ok ()
let received_consensus_operation state update_state consensus_operation
    signature =
  let open Protocol.Operation in
  let%assert () =
    ( `Invalid_signature,
      Consensus.verify state.Node.identity.key signature consensus_operation )
  in
  let operation = Consensus consensus_operation in
  let (_ : State.t) =
    update_state
      (let open Node in
      { state with pending_operations = operation :: state.pending_operations })
  in
  Ok ()
let find_block_by_hash state hash =
  Block_pool.find_block ~hash state.Node.block_pool
let find_block_level state = state.State.protocol.block_height
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
  | None -> Networking.Withdraw_proof.Unknown_operation
  | Some (Receipt_tezos_withdraw withdrawal_handle) ->
    let last_block_hash = state.Node.protocol.last_block_hash in
    let withdrawal_handles_hash =
      match
        Block_pool.find_block ~hash:last_block_hash state.Node.block_pool
      with
      | None -> assert false
      | Some block -> block.Block.withdrawal_handles_hash in
    let proof =
      state.Node.protocol.core_state
      |> Core.State.ledger
      |> Ledger.withdrawal_handles_find_proof withdrawal_handle in
    Ok { withdrawal_handles_hash; withdrawal_handle; proof }
let request_ticket_balance state ~ticket ~address =
  state.Node.protocol.core_state
  |> Core.State.ledger
  |> Ledger.balance address ticket
let trusted_validators_membership state update_state request =
  let open Networking.Trusted_validators_membership_change in
  let { signature; payload = { address; action } as payload } = request in
  let payload_hash =
    payload |> payload_to_yojson |> Yojson.Safe.to_string |> BLAKE2B.hash in
  let%assert () =
    ( `Invalid_signature_author,
      Key_hash.compare state.Node.identity.t (Signature.address signature) = 0
    ) in
  let%assert () =
    (`Failed_to_verify_payload, payload_hash |> Signature.verify ~signature)
  in
  let new_validators =
    match action with
    | Add ->
      Trusted_validators_membership_change.Set.add { action = Add; address }
        state.Node.trusted_validator_membership_change
    | Remove ->
      Trusted_validators_membership_change.Set.add
        { action = Remove; address }
        state.Node.trusted_validator_membership_change in
  let (_ : State.t) =
    update_state
      { state with trusted_validator_membership_change = new_validators } in
  Lwt.async (fun () ->
      state.persist_trusted_membership_change
        (new_validators |> Trusted_validators_membership_change.Set.elements));
  Ok ()
