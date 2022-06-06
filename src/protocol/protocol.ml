open Helpers
open Core_deku
include Exn_noop
module Signature = Protocol_signature
module Wallet = Wallet
module Ledger = Ledger
module Block = Block
module Operation = Protocol_operation
include Protocol_state
open Protocol_operation

let maximum_old_block_height_operation = 60L

let maximum_stored_block_height = 75L

let apply_core_tezos_operation state tezos_operation =
  let%assert () =
    ( `Duplicated_operation,
      not
        (Tezos_operation_set.mem tezos_operation state.included_tezos_operations)
    ) in
  let included_tezos_operations =
    Tezos_operation_set.add tezos_operation state.included_tezos_operations
  in
  let core_state =
    Core_deku.State.apply_tezos_operation state.core_state tezos_operation in
  Ok { state with core_state; included_tezos_operations }

let apply_core_tezos_operation state tezos_operation =
  match apply_core_tezos_operation state tezos_operation with
  | Ok state -> state
  | Error `Duplicated_operation -> state

let apply_core_user_operation state user_operation =
  let Core_user.{ hash; key = _; signature = _; nonce = _; block_height; data }
      =
    user_operation in
  let%assert () = (`Block_in_the_future, block_height <= state.block_height) in
  let%assert () =
    ( `Old_operation,
      Int64.add block_height maximum_old_block_height_operation
      > state.block_height ) in
  let%assert () =
    ( `Duplicated_operation,
      not (User_operation_set.mem user_operation state.included_user_operations)
    ) in
  let included_user_operations =
    User_operation_set.add user_operation state.included_user_operations in
  let core_state, receipt =
    Core_deku.State.apply_user_operation state.core_state hash data in
  Ok ({ state with core_state; included_user_operations }, receipt)

let apply_core_user_operation state tezos_operation =
  match apply_core_user_operation state tezos_operation with
  | Ok (state, receipts) -> (state, receipts)
  | Error (`Block_in_the_future | `Old_operation | `Duplicated_operation) ->
    (state, None)

let apply_validators_operation :
    Protocol_state.t -> Validator.Actor.Validators_action.t -> Protocol_state.t
    =
 fun state validators_op ->
  let validators = state.validators_actor in
  let validators = Validator.Actor.process_operation validators_op validators in
  { state with validators_actor = validators }

let is_next state block =
  Int64.add state.block_height 1L = block.Block.block_height
  && state.last_block_hash = block.previous_hash

let apply_operation :
    Protocol_state.t * (Crypto.BLAKE2B.t * State.receipt) list ->
    Protocol_operation.t ->
    Protocol_state.t * (Crypto.BLAKE2B.t * State.receipt) list =
 fun (state, receipts) operation ->
  match operation with
  | Core_tezos tezos_operation ->
    let state = apply_core_tezos_operation state tezos_operation in
    (state, receipts)
  | Core_user user_operation ->
    let state, receipt = apply_core_user_operation state user_operation in
    let receipts =
      match receipt with
      | Some receipt -> (user_operation.hash, receipt) :: receipts
      | None -> receipts in
    (state, receipts)
  | Validators_action validators_op ->
    let state = apply_validators_operation state validators_op in
    (state, receipts)

let apply_block state block =
  Log.info "block: %Ld" block.Block.block_height;
  let state, receipts =
    List.fold_left apply_operation (state, []) block.operations in
  let state =
    {
      state with
      included_user_operations =
        state.included_user_operations
        |> User_operation_set.filter (fun op ->
               Int64.sub state.block_height op.block_height
               <= maximum_stored_block_height);
    } in
  ( {
      state with
      block_height = block.block_height;
      validators_actor =
        Validator.Actor.update_block_proposer block.author
          state.validators_actor;
      last_block_hash = block.hash;
      last_state_root_update =
        (match block.state_root_hash <> state.state_root_hash with
        | true -> Unix.time ()
        | false -> state.last_state_root_update);
      last_applied_block_timestamp = Unix.time ();
      state_root_hash = block.state_root_hash;
    },
    receipts )

let make ~initial_block =
  let empty =
    {
      core_state = Core_deku.State.empty;
      included_tezos_operations = Tezos_operation_set.empty;
      included_user_operations = User_operation_set.empty;
      validators_actor = Validator.Actor.empty;
      block_height = Int64.sub initial_block.Block.block_height 1L;
      last_block_hash = initial_block.Block.previous_hash;
      state_root_hash = initial_block.Block.state_root_hash;
      last_state_root_update = 0.0;
      last_applied_block_timestamp = 0.0;
    } in
  apply_block empty initial_block |> fst

let apply_block state block =
  let%assert () = (`Invalid_block_when_applying, is_next state block) in
  let state, result = apply_block state block in
  Ok (state, result)

let get_current_block_producer state =
  if state.last_applied_block_timestamp = 0.0 then
    None
  else
    let diff = Unix.time () -. state.last_applied_block_timestamp in
    let skips = Float.to_int (diff /. 10.0) in
    Validator.Actor.get_new_block_proposer skips state.validators_actor
