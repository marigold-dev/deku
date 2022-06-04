open Helpers
open Crypto
open Protocol
open State
open Is_signable

type signed =
  | Signed
  | Not_signed

type step =
  | Noop
  | Effect            of Effect.t
  | Both              of step * step
  (* verify *)
  | Check_operation   of { operation : Operation.t }
  (* transition *)
  | Append_operation  of { operation : Operation.t }
  (* verify *)
  | Check_block       of { block : Block.t }
  (* transition *)
  | Append_block      of { block : Block.t }
  (* verify *)
  | Check_signature   of {
      hash : BLAKE2B.t;
      signature : Signature.t;
    }
  (* transition *)
  | Append_signature  of {
      hash : BLAKE2B.t;
      signature : Signature.t;
    }
  (* verify *)
  | Is_signed_block   of { hash : BLAKE2B.t }
  (* verify  *)
  | Is_future_block   of {
      signed : signed;
      block : Block.t;
    }
  (* verify *)
  | Is_signable_block of { block : Block.t }
  (* transition *)
  | Sign_block        of { block : Block.t }
  (* verify *)
  | Can_apply_block   of { block : Block.t }
  (* transition *)
  | Apply_block       of { block : Block.t }
  (* verify *)
  | Can_produce_block
  (* transition *)
  | Produce_block

type t = step

(* most small steps are here *)
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

let is_next block state = Protocol.is_next state.protocol block

(* `Next means current_block_height + 1
   `Future means current_block_height + 2 + n
   `Past means current_block_height - n *)
let is_future_block block state =
  if is_next block state then
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

let check_operation ~operation state =
  (* TODO: also check core_user properties such as being old operation *)
  if Operation_map.mem operation state.pending_operations then
    Noop
  else
    Append_operation { operation }

let append_operation ~operation state =
  let current_time = Unix.time () in
  let pending_operations =
    Operation_map.add operation current_time state.pending_operations in
  let step =
    match operation with
    | Core_user user_operation ->
      Effect (Broadcast_user_operation { user_operation })
    | Consensus _
    | Core_tezos _ ->
      Noop in
  ({ state with pending_operations }, step)

let check_block ~block state =
  let%ok () =
    is_valid_block state block
    |> Result.map_error (fun msg -> `Invalid_block msg) in
  let%assert () =
    (`Already_known_block, not (is_known_block state ~hash:block.Block.hash))
  in
  Ok (Append_block { block })

let append_block ~block state =
  let block_pool = Block_pool.append_block block state.block_pool in
  ({ state with block_pool }, Is_signed_block { hash = block.hash })

let check_signature ~hash ~signature state =
  let%assert () =
    (`Invalid_signature_for_this_hash, Signature.verify ~signature hash) in
  (* TODO: consider edge-cases related to the node being out sync.
     If validators changed and you are out of sync, you will reject valid
     signatures or accept invalid signatures (the node can wait and restart to back in sync by querying
     the Tezos contract) *)
  (* TODO: *)
  let%assert () =
    (`Not_a_validator, is_validator_address state (Signature.address signature))
  in
  (* TODO: this could likely be removed*)
  let%assert () =
    (`Already_known_signature, not (is_known_signature state ~hash ~signature))
  in
  Ok (Append_signature { hash; signature })

let append_signature ~hash ~signature state =
  let block_pool =
    Block_pool.append_signature
      ~signatures_required:(signatures_required state)
      ~hash signature state.block_pool in
  ({ state with block_pool }, Is_signed_block { hash })

let is_signed_block ~hash state =
  if is_signed_block_hash ~hash state then
    match Block_pool.find_block ~hash state.block_pool with
    | Some block -> Is_future_block { signed = Signed; block }
    | None -> Effect (Request_block { hash })
  else
    Noop

(* TODO: this is checking too much*)
let is_future_block ~signed ~block state =
  match (is_future_block block state, signed) with
  | `Past, _ ->
    (* TODO: may be malicious? Also, it may be a historical fork *)
    Noop
  | `Next, _ -> Both (Is_signable_block { block }, Can_apply_block { block })
  | `Future, Signed -> Effect (Request_previous_blocks { block })
  | `Future, Not_signed -> (* TODO: may be malicious? *) Noop

let is_signable_block ~block state =
  if is_signable block state then Sign_block { block } else Noop

let sign_block ~block state =
  let secret = state.identity.secret in
  (* TODO: rename Block.sign ~key to ~secret *)
  let signature = Block.sign ~key:secret block in
  let hash = block.hash in
  Both
    ( Effect (Broadcast_signature { hash; signature }),
      Check_signature { hash; signature } )

let can_apply_block ~block state =
  (* WHY: sometimes the current machine tries to apply block twice
     in the same stack, because it produced an additional signature.
     Also it's a very good safeguard to have *)
  if
    is_next block state
    && Block_pool.is_signed ~hash:block.hash state.block_pool
  then
    Apply_block { block }
  else
    Noop

let can_produce_block state =
  if is_current_producer state ~key_hash:state.identity.t then
    Produce_block
  else
    Noop
