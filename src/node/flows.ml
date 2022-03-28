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
  | `Not_consensus_operation msg ->
    Format.sprintf "Not_consensus_operation: %s" msg
  | `Invalid_signature -> "Invalid_signature"
  | `Invalid_snapshot_height -> "Invalid_snapshot_height"
  | `Not_all_blocks_are_signed -> "Not_all_blocks_are_signed"
  | `State_root_not_the_expected -> "State_root_not_the_expected"
  | `Snapshots_with_invalid_hash -> "Snapshots_with_invalid_hash"
  | `Signed_by_unauthorized_validator -> "Signed_by_unauthorized_validator"

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

let get_consensus = (ref (fun _ -> assert false) : (unit -> Tendermint.t) ref)
let set_consensus = (ref (fun _ -> assert false) : (Tendermint.t -> unit) ref)

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
  (* Lwt.async (fun () -> *)
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
  | Error _err ->
    Printf.eprintf "Error while requesting block!";
    await ()
(* ) *)

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
  (* Lwt.async (fun () -> *)
  let%await snapshot = request_protocol_snapshot 0 in
  (match load_snapshot snapshot with
  | Ok _ -> ()
  | Error err -> print_error err);
  await ()

let request_previous_blocks state block =
  let%await () =
    if
      block_matches_current_state_root_hash state block
      || block_matches_next_state_root_hash state block
    then
      request_block ~hash:block.Block.previous_hash
    else (
      (* if not !pending then *)
      pending := true;
      (* let%await () = *)
      request_protocol_snapshot ()
      (* in
         request_block ~hash:block.Block.previous_hash *)) in
  await @@ !get_state ()

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
let try_to_commit_state_hash ~prev_validators state block round signatures =
  let open Node in
  let signatures_map =
    signatures
    (* FIXME? |> Signatures.to_list *)
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
        ~block_payload_hash:block.payload_hash ~block_round:round
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
        block block.Block.consensus_round
        (Signatures.to_list signatures)
    | _ -> ());
  match
    Block_pool.find_next_block_to_apply ~hash:block.Block.hash state.block_pool
  with
  | Some block ->
    let state = try_to_sign_block state update_state block in
    try_to_apply_block state update_state block
  | None -> Ok ()
(* try_to_produce_block state update_state *)

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
    (* let state = try_to_sign_block state update_state block in *)
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
      ignore (request_previous_blocks state block);
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
    let _ = request_block ~hash in
    (* FIXME: this is a hack. *)
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
  let current_time = Unix.time () in
  let pending_operation =
    Node.{ requested_at = current_time; operation = Core_tezos tezos_operation }
  in
  let (_ : State.t) =
    update_state
      (let open Node in
      {
        state with
        pending_operations = pending_operation :: state.pending_operations;
      }) in
  ()
let received_user_operation state update_state user_operation =
  let open Protocol.Operation in
  let operation = Core_user user_operation in
  let operation_exists =
    List.exists
      (fun pending_operation ->
        equal pending_operation.Node.operation operation)
      state.Node.pending_operations in

  let current_time = Unix.time () in
  let pending_operation =
    Node.{ requested_at = current_time; operation = Core_user user_operation }
  in
  if not operation_exists then (
    Lwt.async (fun () ->
        Networking.broadcast_user_operation_gossip state { user_operation });
    let (_ : State.t) =
      update_state
        (let open Node in
        {
          state with
          pending_operations = pending_operation :: state.pending_operations;
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
  let current_time = Unix.time () in
  let pending_operation =
    Node.
      { requested_at = current_time; operation = Consensus consensus_operation }
  in
  let (_ : State.t) =
    update_state
      (let open Node in
      {
        state with
        pending_operations = pending_operation :: state.pending_operations;
      }) in
  Ok ()

let is_authorized_validator state ~signature =
  let validators = state.Node.protocol.validators in
  let public_key = Signature.public_key signature in
  let key_hash = Key_hash.of_key public_key in
  Validators.is_validator validators key_hash

let already_committed state block =
  state.Node.protocol.Protocol.last_block_hash = block.Block.hash

(** Apply a block to Deku and commit the *previous previous block* to Tezos; does not check that the block should indeed be committed. *)
let commit state update_state ~block ~hash ~height ~round:_ =
  let prev_protocol = state.Node.protocol in
  let is_new_state_root_hash =
    not
      (BLAKE2B.equal state.Node.protocol.state_root_hash
         block.Block.state_root_hash) in

  (* For security reason, we commit to tezos a block later *)
  (* FIXME: this has to change when validator governance is live. *)
  let prev_height = Int64.sub height 1L in
  let () =
    match Tendermint.get_block_opt (!get_consensus ()) prev_height with
    | None -> ()
    | Some (b, round) ->
      let previous_hash = b.Block.hash in
      let signatures =
        Block_pool.find_signatures ~hash:previous_hash state.Node.block_pool
      in
      let signatures =
        match signatures with
        | None -> []
        | Some sigs -> Signatures.to_list sigs in
      try_to_commit_state_hash ~prev_validators:prev_protocol.validators state b
        (* FIXME? previous_hash *)
        round signatures in
  let%ok state = apply_block state update_state block in
  (* Save the state *)
  write_state_to_file (state.Node.data_folder ^ "/state.bin") state.protocol;
  let state = clean state update_state block in
  if is_new_state_root_hash then
    write_state_to_file
      (state.data_folder ^ "/prev_epoch_state.bin")
      prev_protocol;
  let signatures =
    match Block_pool.find_signatures ~hash state.Node.block_pool with
    | Some signatures -> signatures
    (* We already checked that the block is correct and signed (PRECOMMITted) *)
    | None -> assert false in
  (* let signatures = Staging_area.get state.Node.staging_area hash height round in *)
  let snapshots =
    Snapshots.append_block ~pool:state.Node.block_pool (block, signatures)
      state.snapshots in
  ignore (update_state { state with snapshots });
  Result.ok ()

(** Manages the block pool and signatures when we received a Precommit op (applies changes post-consensus) *)
let received_precommit_block state update_state ~block ~sender:_ ~hash
    ~hash_signature =
  let module CI = Tendermint_internals in
  let height, _ = (block.Block.block_height, block.Block.consensus_round) in
  (* We check for the signature of the *hash* of the received hash, as this
     is also what Tezos does. *)
  let%assert () =
    ( `Already_known_signature,
      not (is_known_signature state ~hash ~signature:hash_signature) ) in

  (* TODO: this saves signatures for blocks/hashes that are not valid for Tendermint.
     - DDOS risk
     - use case?  (if none: just save signatures when Tendermint agrees/is undecided) (possible use case: slashing?)*)
  let state =
    append_signature state update_state ~hash ~signature:hash_signature in
  match Tendermint.get_block_opt (!get_consensus ()) height with
  | Some (block, _) when not (block.Block.hash = hash) ->
    Result.Error
      (`Invalid_block
        (Printf.sprintf
           "Block hash does not match decision made by consensus, %s"
           (Crypto.BLAKE2B.to_string block.Block.hash)))
  (* TODO: move | Some (block, _round) when not (check_state_root_hash block) ->
     Result.error `Invalid_state_root_hash *)
  | Some (block, _) ->
    (* Consensus has been reached normally for the received block *)
    (* The block has been decided on, is valid, and we have enough signatures to commit *)
    (* let state =
       append_signature state update_state ~hash ~signature:hash_signature in *)
    if not (already_committed state block) then
      let state = Building_blocks.add_block_to_pool state update_state block in
      (* commit ~block ~hash ~height ~round state update_state *)
      block_added_to_the_pool state update_state block
    else
      Ok ()
  | None ->
    (* Consensus has not been reached yet *)
    Ok ()

let received_consensus_step state update_state operation sender hash
    block_signature message_signature =
  (* DEBUG: This can be quite useful:
     Tendermint_internals.debug state
       (Printf.sprintf "received consensus operation %s from %s"
          (Tendermint_internals.string_of_op operation)
          (Tendermint_internals.short sender)); *)
  let open Tendermint in
  let open Tendermint_internals in
  (* TODO: put everything in Tendermint *)
  let _check_state_root_hash block =
    block_matches_current_state_root_hash state block
    || block_matches_next_state_root_hash state block in

  let*? () =
    let hash = hash_of_consensus_op operation sender in
    ensure
      ( `Invalid_signature_for_this_hash,
        Signature.verify ~signature:message_signature hash ) in

  let*? () =
    ensure
      ( `Signed_by_unauthorized_validator,
        is_authorized_validator state ~signature:message_signature ) in

  (* FIXME: ConsensusStep2 this is a poor man's fast sync protocol. This should be implemented somewhere else, as
     it creates a vulnerability and Tendermint can get stuck as it is implemented now. *)
  let* state, _consensus =
    let height = state.Node.protocol.Protocol.block_height in
    match operation with
    | ProposalOP (height', _, Block b, _) when height' > Int64.add height 1L ->
      let* state = request_previous_blocks state b in
      (* Start Tendermint process at the right height *)
      let current_height = height' in
      let consensus' = Tendermint.make state current_height in
      !set_consensus consensus';
      assert (Int64.add state.Node.protocol.Protocol.block_height 1L >= height');
      Lwt.return (state, consensus')
    | _ -> Lwt.return (state, !get_consensus ()) in

  let*? () =
    let op_height = Tendermint_internals.height operation in
    let own_height = state.Node.protocol.Protocol.block_height in
    ensure
      ( `Not_consensus_operation
          (Printf.sprintf "Wrong height for operation: %Ld, expected height %Ld"
             op_height own_height),
        Int64.abs (Int64.sub op_height own_height) <= 1L ) in

  (* FIXME: @arthur, this is not working at the moment
     let*? () =
       ensure (`State_root_not_the_expected,
       value_of_op operation |> on_block ~nil_default:true check_state_root_hash)
     in
  *)

  (* TODO: ConsensusStep2 if received a block, check state root hash *)
  match is_valid_consensus_op state operation with
  | Ok () ->
    (* TODO: ConsensusStep2, check if already seen this message? AKA enforce unique in input_log? *)
    (* TODO: ConsensusStep2: add and check sender signature? *)
    let consensus = !get_consensus () in
    let%await consensus = add_consensus_op consensus sender operation in

    (* Execute the consensus step and updates the consensus state.
       In the case this step is a PrecommitOP, we may need to commit the whole block
       and then reexecute the consensus to decide if we propose a new block! *)
    let consensus = exec_consensus consensus in
    !set_consensus consensus;
    Lwt.return
      begin
        match operation with
        | PrecommitOP (_, _, Block b) ->
          (* Implicitly updates the node state *)
          let%ok () =
            received_precommit_block state update_state ~block:b ~sender ~hash
              ~hash_signature:block_signature in
          (* Not very elegant. We'll probably hide this global state in the future. *)
          let state = !get_state () in
          let consensus = { consensus with Tendermint.node_state = state } in

          (* Rexec the consensus in case we need to send a proposal *)
          let consensus = exec_consensus consensus in
          !set_consensus consensus;
          Ok ()
        | _ -> Ok ()
      end
  | Error msg -> Lwt.return (Error (`Not_consensus_operation msg))

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
