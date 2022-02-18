open Helpers
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
  Some (current_producer.address = key_hash)
let minimum_signable_time_between_epochs = 10.0
let maximum_signable_time_between_epochs = 20.0
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
let is_signable state block =
  let {
    Node.trusted_validator_membership_change;
    protocol = { last_seen_membership_change_timestamp; _ };
    _;
  } =
    state in
  let current_time = Unix.time () in
  let next_allowed_membership_change_timestamp =
    last_seen_membership_change_timestamp +. (24. *. 60. *. 60.) in
  let is_trusted_operation operation =
    match operation with
    | Protocol.Operation.Core_tezos _ ->
      List.exists
        (fun op -> Protocol.Operation.equal op operation)
        state.pending_operations
    | Core_user _ -> true
    | Consensus consensus_operation -> (
      current_time > next_allowed_membership_change_timestamp
      &&
      match consensus_operation with
      | Add_validator validator ->
        Trusted_validators_membership_change.Set.mem
          { address = validator.address; action = Add }
          trusted_validator_membership_change
      | Remove_validator validator ->
        Trusted_validators_membership_change.Set.mem
          { address = validator.address; action = Remove }
          trusted_validator_membership_change) in
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

let produce_block state =
  let start_new_epoch =
    should_start_new_epoch state.Node.protocol.last_state_root_update
      (Unix.time ()) in
  let next_state_root_hash =
    if start_new_epoch then
      let%some snapshot = Snapshots.get_next_snapshot state.snapshots in
      Some snapshot.hash
    else
      None in
  Block.produce ~state:state.Node.protocol ~author:state.identity.t
    ~next_state_root_hash ~operations:state.pending_operations
let is_valid_block_height state block_height =
  block_height >= 1L && block_height <= state.Node.protocol.block_height
let signatures_required state =
  let number_of_validators = Validators.length state.Node.protocol.validators in
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
let clean state update_state block =
  let operation_is_in_block operation =
    List.exists (fun op -> Operation.equal op operation) block.Block.operations
  in
  let pending_operations =
    List.find_all
      (fun operation -> not (operation_is_in_block operation))
      state.State.pending_operations in
  let trusted_validator_membership_change =
    List.fold_left
      (fun trusted_validator_membership_change operation ->
        match operation with
        | Operation.Core_tezos _
        | Core_user _ ->
          trusted_validator_membership_change
        | Consensus (Add_validator validator) ->
          Trusted_validators_membership_change.Set.remove
            { address = validator.address; action = Add }
            trusted_validator_membership_change
        | Consensus (Remove_validator validator) ->
          Trusted_validators_membership_change.Set.remove
            { address = validator.address; action = Remove }
            state.Node.trusted_validator_membership_change)
      state.Node.trusted_validator_membership_change block.operations in
  Lwt.async (fun () ->
      trusted_validator_membership_change
      |> Trusted_validators_membership_change.Set.elements
      |> state.persist_trusted_membership_change);
  update_state
    { state with trusted_validator_membership_change; pending_operations }
let broadcast_signature state ~hash ~signature =
  Lwt.async (fun () -> Networking.broadcast_signature state { hash; signature })
let broadcast_block_and_signature state ~block ~signature =
  Lwt.async (fun () ->
      let%await () = Lwt_unix.sleep 1.0 in
      Networking.broadcast_block_and_signature state { block; signature })
let find_random_validator_uri state =
  let random_int v = v |> Int32.of_int |> Random.int32 |> Int32.to_int in
  let validators = Validators.to_list state.Node.protocol.validators in
  let rec safe_validator_uri () =
    let validator = List.nth validators (random_int (List.length validators)) in
    if state.Node.identity.t = validator.address then
      safe_validator_uri ()
    else
      match
        Node.Address_map.find_opt validator.address state.validators_uri
      with
      | Some uri -> uri
      | None -> safe_validator_uri () in
  safe_validator_uri ()
