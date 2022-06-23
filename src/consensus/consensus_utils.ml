open Helpers
open Crypto
open Protocol
open State

let block_matches_current_state_root_hash state block =
  BLAKE2B.equal block.Block.state_root_hash state.protocol.state_root_hash

let block_matches_next_state_root_hash state block =
  match Snapshots.get_next_snapshot state.snapshots with
  | Some { hash = next_state_root_hash; _ } ->
    BLAKE2B.equal block.Block.state_root_hash next_state_root_hash
  | None -> false

let is_validator_address state address =
  List.exists
    (fun validator -> Key_hash.equal validator.Validators.address address)
    (Validators.to_list state.protocol.validators)

let is_known_block state ~hash =
  match Block_pool.find_block ~hash state.block_pool with
  | Some _block -> true
  | None -> false

let is_known_signature state ~hash ~signature =
  match Block_pool.find_signatures ~hash state.block_pool with
  | Some signatures -> Signatures.mem signature signatures
  | None -> false

let signatures_required state =
  let number_of_validators = Validators.length state.protocol.validators in
  let open Float in
  to_int (ceil (of_int number_of_validators *. (2.0 /. 3.0)))

let is_next state block = Protocol.is_next state.protocol block

(* `Next means current_block_height + 1 `Future means current_block_height + 2 +
   n `Past means current_block_height - n *)
let is_future_block state block =
  if is_next state block then
    `Next
  else if block.block_height > state.protocol.block_height then
    `Future
  else
    `Past

let is_signed_block_hash ~hash state =
  match Block_pool.find_signatures ~hash state.block_pool with
  | Some signatures -> Signatures.is_signed signatures
  | None -> false

let is_valid_block state block =
  let is_all_operations_properly_signed _block = true in
  let%assert () =
    ( Printf.sprintf
        "new block has a lower block height (%Ld) than the current state (%Ld)"
        block.Block.block_height state.protocol.block_height,
      block.Block.block_height >= state.protocol.block_height ) in
  let%assert () =
    ( "some operation in the block is not properly signed",
      is_all_operations_properly_signed block ) in
  Ok ()
