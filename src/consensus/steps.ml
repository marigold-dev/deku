open Helpers
open Crypto
open Protocol
open State
open Is_signable
open Consensus_utils

type signed =
  | Signed
  | Not_signed

type step =
  | Noop
  | Effect                    of Effect.t
  | Both                      of step * step
  (* verify *)
  | Check_operation           of { operation : Operation.t }
  (* transition *)
  | Append_operation          of { operation : Operation.t }
  (* verify *)
  | Check_block               of { block : Block.t }
  (* transition *)
  | Append_block              of { block : Block.t }
  (* verify *)
  | Check_signature           of {
      hash : BLAKE2B.t;
      signature : Signature.t;
    }
  (* transition *)
  | Append_signature          of {
      hash : BLAKE2B.t;
      signature : Signature.t;
    }
  (* verify *)
  | Is_signed_block           of { hash : BLAKE2B.t }
  (* verify  *)
  | Is_future_block           of {
      signed : signed;
      block : Block.t;
    }
  (* verify *)
  | Is_signable_block         of { block : Block.t }
  (* transition *)
  | Sign_block                of { block : Block.t }
  (* verify *)
  | Can_apply_block           of { block : Block.t }
  (* transition *)
  | Apply_block               of { block : Block.t }
  (* verify *)
  | Can_produce_block
  (* transition *)
  | Produce_block
  (* verify *)
  | Check_validator_change    of {
      payload : Network.Trusted_validators_membership_change.payload;
      signature : Signature.t;
    }
  (* transition *)
  | Allow_to_add_validator    of { key_hash : Key_hash.t }
  (* transition *)
  | Allow_to_remove_validator of { key_hash : Key_hash.t }

type t = step

(* most small steps are here *)

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
  Log.info "Checking %a" Block.pp block;
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
  let signed = if is_signed_block_hash ~hash state then Signed else Not_signed in
  match (Block_pool.find_block ~hash state.block_pool, signed) with
  | Some block, _ -> Is_future_block { signed; block }
  | None, Signed -> Effect (Request_block { hash })
  | None, Not_signed -> Noop

(* TODO: this is checking too much*)
let is_future_block ~signed ~block state =
  match (is_future_block state block, signed) with
  | `Past, _ ->
    (* TODO: may be malicious? Also, it may be a historical fork *)
    Noop
  | `Next, _ -> Both (Is_signable_block { block }, Can_apply_block { block })
  | `Future, Signed -> Effect (Request_previous_blocks { block })
  | `Future, Not_signed -> (* TODO: may be malicious? *) Noop

let is_signable_block ~block state =
  if is_signable state block then Sign_block { block } else Noop

let sign_block ~block state =
  let secret = state.identity.secret in
  (* TODO: rename Block.sign ~key to ~secret *)
  Log.info "Signing: %a" Block.pp block;
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
    is_next state block
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

let check_validator_change ~payload ~signature state =
  let open Network.Trusted_validators_membership_change in
  let payload_hash =
    payload |> payload_to_yojson |> Yojson.Safe.to_string |> BLAKE2B.hash in
  let%assert () =
    ( `Invalid_signature_author,
      Key_hash.compare state.identity.t (Signature.address signature) = 0 )
  in
  let%assert () =
    (`Failed_to_verify_payload, payload_hash |> Signature.verify ~signature)
  in

  let { action; address = key_hash } = payload in
  match action with
  | Add -> Ok (Allow_to_add_validator { key_hash })
  | Remove -> Ok (Allow_to_remove_validator { key_hash })

let allow_to_add_validator ~key_hash state =
  let trusted_validator_membership_change =
    Trusted_validators_membership_change.Set.add
      { action = Add; address = key_hash }
      state.trusted_validator_membership_change in
  let state = { state with trusted_validator_membership_change } in
  let effect =
    Effect.Persist_trusted_membership_change
      { trusted_validator_membership_change } in
  (state, Effect effect)

let allow_to_remove_validator ~key_hash state =
  let trusted_validator_membership_change =
    Trusted_validators_membership_change.Set.add
      { action = Remove; address = key_hash }
      state.trusted_validator_membership_change in

  let state = { state with trusted_validator_membership_change } in
  let effect =
    Effect.Persist_trusted_membership_change
      { trusted_validator_membership_change } in
  (state, Effect effect)
