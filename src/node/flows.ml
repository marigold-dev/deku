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

let print_error err =
  let open Format in
  eprintf "\027[31m";
  eprintf "Error: ";
  (match err with
  | `Added_block_has_lower_block_height ->
    eprintf "Added block has lower block height"
  | `Added_block_not_signed_enough_to_desync ->
    eprintf "Added_block_not_signed_enough_to_desync"
  | `Added_signature_not_signed_enough_to_request ->
    eprintf "Added_signature_not_signed_enough_to_request"
  | `Already_known_block -> eprintf "Already_known_block"
  | `Already_known_signature -> eprintf "Already_known_signature"
  | `Block_not_signed_enough_to_apply ->
    eprintf "Block_not_signed_enough_to_apply"
  | `Failed_to_verify_payload -> eprintf "Failed to verify payload signature"
  | `Invalid_address_on_main_operation ->
    eprintf "Invalid_address_on_main_operation"
  | `Invalid_block string -> eprintf "Invalid_block(%s)" string
  | `Invalid_block_when_applying -> eprintf "Invalid_block_when_applying"
  | `Invalid_nonce_signature -> eprintf "Invalid_nonce_signature"
  | `Invalid_signature_author -> eprintf "Invalid_signature_author"
  | `Invalid_signature_for_this_hash ->
    eprintf "Invalid_signature_for_this_hash"
  | `Signed_by_unauthorized_validator ->
    eprintf "Signed_by_unauthorized_validator"
  | `Consensus_not_reached_yet -> eprintf "Consensus_not_reached_yet"
  | `Invalid_state_root_hash -> eprintf "Invalid_state_root_hash"
  | `Not_current_block_producer -> eprintf "Not_current_block_producer"
  | `Not_a_json -> eprintf "Invalid json"
  | `Not_a_valid_request err -> eprintf "Invalid request: %s" err
  | `Pending_blocks -> eprintf "Pending_blocks"
  | `Unknown_uri -> eprintf "Unknown_uri"
  | `Not_a_user_opertaion -> eprintf "Not_a_user_opertaion"
  | `Not_consensus_operation -> eprintf "Not_consensus_operation"
  | `Invalid_signature -> eprintf "Invalid_signature"
  | `Invalid_snapshot_height -> eprintf "Invalid_snapshot_height"
  | `Not_all_blocks_are_signed -> eprintf "Not_all_blocks_are_signed"
  | `State_root_not_the_expected -> eprintf "State_root_not_the_expected"
  | `Snapshots_with_invalid_hash -> eprintf "Snapshots_with_invalid_hash");
  eprintf "\027[m\n%!"

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
;;
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
      ~last_block:snapshot_data.last_block (!get_state ()) in
  Ok (!set_state state)
let request_protocol_snapshot () =
  Lwt.async (fun () ->
      let%await snapshot = request_protocol_snapshot 0 in
      let _result = load_snapshot snapshot in
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

let try_to_commit_state_hash ~prev_validators state block round signatures =
  let open Node in
  let signatures_map =
    signatures
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
      (* FIXME: this can't be good *)
      Tezos_interop.Consensus.commit_state_hash
        ~context:state.Node.interop_context ~block_height:block.block_height
        ~block_round:round ~block_payload_hash:block.payload_hash
        ~handles_hash:block.handles_hash ~state_hash:block.state_root_hash
        ~validators ~signatures)

let signatures_required state =
  let number_of_validators = Validators.length state.Node.protocol.validators in
  let open Float in
  to_int (ceil (2. *. (of_int number_of_validators -. 1.) /. 3.) +. 1.)

let enough_signatures state hash height round =
  let staging_area = state.Node.staging_area in
  Staging_area.nb_of_signatures staging_area hash height round
  >= signatures_required state

let is_authorized_validator state ~signature =
  let validators = state.Node.protocol.validators in
  let public_key = Signature.public_key signature in
  let key_hash = Key_hash.of_key public_key in
  Validators.is_validator validators key_hash

(** Apply a block to Deku and commit the *previous previous block* to Tezos; does not check that the block should indeed be committed. *)
let commit state update_state ~block ~hash ~height ~round =
  let prev_protocol = state.Node.protocol in
  let is_new_state_root_hash =
    not
      (BLAKE2B.equal state.Node.protocol.state_root_hash
         block.Block.state_root_hash) in

  (* For security reason, we commit to tezos a block later *)
  (* FIXME: this has to change when validator governance is live. *)
  let () =
    match Tendermint.previous_block (!get_consensus ()) height with
    | None -> ()
    | Some (b, round) ->
      let signatures =
        Staging_area.get state.Node.staging_area b.hash (Int64.sub height 1L)
          round in
      if List.length signatures = 0 then
        prerr_endline
          "This should never be printed, there should be signatures at this \
           time.";
      try_to_commit_state_hash ~prev_validators:prev_protocol.validators state b
        round signatures in
  let%ok state = apply_block state update_state block in
  (* Save the state *)
  write_state_to_file (state.Node.data_folder ^ "/state.bin") state.protocol;
  let state = clean state update_state block in
  if is_new_state_root_hash then
    write_state_to_file
      (state.data_folder ^ "/prev_epoch_state.bin")
      prev_protocol;
  let signatures = Staging_area.get state.Node.staging_area hash height round in
  (* We already checked that the block is correct and signed (PRECOMMITted) *)
  let snapshots = Snapshots.append_block block signatures state.snapshots in
  ignore (update_state { state with snapshots });
  Result.ok ()

(** Receives a signature for a block after a Tendermint PRECOMMIT step for given height and round *)
let received_precommit_block state update_state ~consensus_op ~sender ~hash
    ~hash_signature ~signature =
  let module CI = Tendermint_internals in
  CI.debug state "Received signature";
  let height, round =
    ( Tendermint.height_from_op consensus_op,
      Tendermint.round_from_op consensus_op ) in
  let%assert () =
    ( `Invalid_signature_for_this_hash,
      Signature.verify ~signature:hash_signature hash ) in
  let s1 = Tendermint_internals.string_of_op consensus_op in
  let s2 = Crypto.Key_hash.to_string sender in
  let message_hash = Crypto.BLAKE2B.hash (s1 ^ s2) in
  let%assert () =
    (`Invalid_signature_for_this_hash, Signature.verify ~signature message_hash)
  in
  let%assert () =
    (`Signed_by_unauthorized_validator, is_authorized_validator state ~signature)
  in
  let%assert () =
    ( `Already_known_signature,
      not (is_known_signature state ~hash ~signature ~height ~round) ) in
  let check_state_root_hash block =
    block_matches_current_state_root_hash state block
    || block_matches_next_state_root_hash state block in
  match Tendermint.is_decided_on (!get_consensus ()) height with
  | Some (block, _round) when block.hash <> hash ->
    Result.Error
      (`Invalid_block "Block hash does not match decision made by consensus")
  | Some (block, _round) when not (check_state_root_hash block) ->
    Result.error `Invalid_state_root_hash
  | None ->
    let _ =
      append_signature state update_state ~hash ~signature ~height ~round in
    Ok ()
  | Some (block, _) ->
    (* The block has been decided on, is valid, and we have enough signatures to commit *)
    let _ =
      append_signature state update_state ~hash ~signature ~height ~round in
    commit state update_state ~block ~hash ~height ~round

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

let received_consensus_step state update_state sender operation =
  Tendermint_internals.debug state
    (Printf.sprintf "received consensus step %s"
       (Tendermint_internals.string_of_op operation));
  let%ok () =
    Tendermint.is_valid_consensus_op state operation
    |> Result.map_error (fun _msg -> `Not_consensus_operation) in

  (* TODO: Tendermint, check if already seen this message? AKA enforce unique in input_log? *)
  (* TODO: Tendermint: add and check sender signature? *)
  let consensus = !get_consensus () in
  let consensus =
    Tendermint.add_consensus_op consensus update_state sender operation in

  (* Execute the consensus steps *)
  (* TODO: Tendermint: not sure we should do this here *)
  let consensus = Tendermint.exec_consensus consensus in
  !set_consensus consensus;
  Ok ()

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
  | Some (Receipt_tezos_withdraw handle) ->
    let last_block =
      Tendermint.get_block (!get_consensus ()) state.Node.protocol.block_height
    in
    let handles_hash = last_block.Block.handles_hash in
    let proof =
      state.Node.protocol.core_state
      |> Core.State.ledger
      |> Ledger.handles_find_proof handle in
    Ok { handles_hash; handle; proof }
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
