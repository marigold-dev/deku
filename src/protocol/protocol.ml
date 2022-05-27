open Helpers
open Core
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
    Core.State.apply_tezos_operation state.core_state tezos_operation in
  Ok { state with core_state; included_tezos_operations }
let apply_core_tezos_operation state tezos_operation =
  match apply_core_tezos_operation state tezos_operation with
  | Ok state -> state
  | Error `Duplicated_operation -> state
let apply_core_user_operation state user_operation =
  let Core_user.
        { hash = _; key = _; signature = _; nonce = _; block_height; data } =
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
    Core.State.apply_user_operation state.core_state data in
  Ok ({ state with core_state; included_user_operations }, receipt)
let apply_core_user_operation state tezos_operation =
  match apply_core_user_operation state tezos_operation with
  | Ok (state, receipts) -> (state, receipts)
  | Error (`Block_in_the_future | `Old_operation | `Duplicated_operation) ->
    (state, None)

(* Validators ops coming through network, see Validators prenode. *)
let apply_validators_operation :
    Protocol_state.t ->
    Prenode.Message.t ->
    Protocol_state.t * Prenode.Message.t list =
 fun state msg ->
  let validators_prenode, msgs =
    Prenode.Validators_Prenode.process_message msg state.validators_prenode
  in
  let last_seen_membership_change_timestamp = Unix.time () in
  ( { state with validators_prenode; last_seen_membership_change_timestamp },
    msgs )

let is_next state block =
  Int64.add state.block_height 1L = block.Block.block_height
  && state.last_block_hash = block.previous_hash

let apply_protocol_operations :
    Protocol_state.t * (Crypto.BLAKE2B.t * State.receipt) list ->
    Prenode.Message.t ->
    (Protocol_state.t * (Crypto.BLAKE2B.t * State.receipt) list)
    * Prenode.Message.t list =
 fun (state, receipts) msg ->
  let deku_op = Prenode.Message.get_operation msg in
  match deku_op.operation_family with
  | Validators ->
    let state, msgs = apply_validators_operation state msg in
    ((state, receipts), msgs)
  | _ -> ((state, receipts), [])

(* TODO: remove after full pollinate integration *)
let apply_old_operation (state, receipts) operation =
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
  | _ -> (state, receipts)
(* FIXME: remove after full pollinate integration*)

let apply_operation :
    Protocol_state.t * (Crypto.BLAKE2B.t * State.receipt) list ->
    (Prenode.Message.t, Operation.t) Either.t ->
    (Protocol_state.t * (Crypto.BLAKE2B.t * State.receipt) list)
    * Prenode.Message.t list =
 fun (state, receipts) msg ->
  match msg with
  | Left msg -> apply_protocol_operations (state, receipts) msg
  | Right msg ->
    let state, receipts = apply_old_operation (state, receipts) msg in
    ((state, receipts), [])

(* TODO: update using new protocol operations *)
let apply_block state block =
  Format.eprintf "\027[32mblock: %Ld\027[m\n%!" block.Block.block_height;
  let state, receipts =
    List.fold_left apply_old_operation (state, []) block.operations in
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
      validators_prenode =
        Prenode.Validators_Prenode.update_block_proposer block.author
          state.validators_prenode;
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
      core_state = Core.State.empty;
      included_tezos_operations = Tezos_operation_set.empty;
      included_user_operations = User_operation_set.empty;
      validators_prenode = Prenode.Validators_Prenode.empty;
      block_height = Int64.sub initial_block.Block.block_height 1L;
      last_block_hash = initial_block.Block.previous_hash;
      state_root_hash = initial_block.Block.state_root_hash;
      last_state_root_update = 0.0;
      last_applied_block_timestamp = 0.0;
      last_seen_membership_change_timestamp = 0.0;
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
    Prenode.Validators_Prenode.get_new_block_proposer skips
      state.validators_prenode
