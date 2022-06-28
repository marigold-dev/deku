#import "assert.mligo" "Assert"
#import "errors.mligo" "Errors"
#import "parameter.mligo" "Parameter"

type blake2b = Parameter.Types.blake2b
type root_hash_action = Parameter.Types.root_hash_action
type validator_keys = Parameter.Types.validator_keys
type signatures = Parameter.Types.signatures
type root_hash_storage = Parameter.Types.root_hash_storage
type validators = Parameter.Types.validators
type vault_storage = Parameter.Types.vault_storage

let root_hash_block_hash (root_hash_update: root_hash_action) =
    let block_hash_structure = {
      block_height = root_hash_update.block_height;
      block_payload_hash = root_hash_update.block_payload_hash;
      state_hash = root_hash_update.state_hash;
      handles_hash = root_hash_update.handles_hash;
      (* TODO: should we do pack of list? *)
      validators_hash = Crypto.blake2b (Bytes.pack root_hash_update.validators)
    } in
    Crypto.blake2b (Bytes.pack block_hash_structure)

let root_hash_check_block_height
  (storage: root_hash_storage)
  (block_height: int) =
    Assert.assert_msg (
      Errors.old_block_hash,
      block_height > storage.current_block_height
    )

let rec root_hash_check_signatures
  (validator_keys, signatures, block_hash, remaining:
    validator_keys * signatures * blake2b * int) : unit =
    match (validator_keys, signatures) with
    (* already signed *)
    | ([], []) ->
      (* TODO: this can be short circuited *)
      if remaining > 0 then
        failwith Errors.not_enough_key_signature_matches
    | ((Some validator_key :: vk_tl), (Some signature :: sig_tl)) ->
      if Crypto.check validator_key signature block_hash
      then
        root_hash_check_signatures (vk_tl, sig_tl, block_hash, (remaining - 1))
      else failwith Errors.bad_signature
    | ((_ :: vk_tl), (None :: sig_tl)) ->
      root_hash_check_signatures (vk_tl, sig_tl, block_hash, remaining)
    | ((None :: vk_tl), (_ :: sig_tl)) ->
      root_hash_check_signatures (vk_tl, sig_tl, block_hash, remaining)
    | (_, _) ->
      failwith Errors.different_size_validators_signatures

let root_hash_check_signatures
  (action: root_hash_action)
  (storage: root_hash_storage)
  (signatures: signatures)
  (block_hash: blake2b) =
    let validators_length = (int (List.length storage.current_validators)) in
    let required_validators = (validators_length * 2) / 3 in
    root_hash_check_signatures (
      action.current_validator_keys,
      signatures,
      block_hash,
      required_validators
    )

let rec root_hash_check_keys
  (validator_keys, validators, block_hash, remaining:
    validator_keys * validators * blake2b * int) : unit =
    match (validator_keys, validators) with
    | ([], []) ->
      if remaining > 0 then
        failwith Errors.not_enough_keys
    | ((Some validator_key :: vk_tl), (validator :: v_tl)) ->
      if (Crypto.hash_key validator_key) = validator then
        root_hash_check_keys (vk_tl, v_tl, block_hash, (remaining - 1))
      else failwith Errors.mismatch_key_hash
    | ((None :: vk_tl), (_ :: v_tl)) ->
      root_hash_check_keys (vk_tl, v_tl, block_hash, remaining)
    | (_, _) ->
      failwith Errors.different_size_keys_validators

let root_hash_check_keys
  (action: root_hash_action)
  (storage: root_hash_storage)
  (block_hash: blake2b) =
    let validators_length = (int (List.length storage.current_validators)) in
    let required_validators = (validators_length * 2) / 3 in
    root_hash_check_keys (
      action.current_validator_keys,
      storage.current_validators,
      block_hash,
      required_validators
    )

let root_hash_main
  (root_hash_update: root_hash_action)
  (storage: root_hash_storage) =
    let block_hash = root_hash_block_hash root_hash_update in
    let block_height = root_hash_update.block_height in
    let state_hash = root_hash_update.state_hash in
    let handles_hash = root_hash_update.handles_hash in
    let validators = root_hash_update.validators in
    let signatures = root_hash_update.signatures in

    let () = root_hash_check_block_height storage block_height in
    let () = root_hash_check_signatures root_hash_update storage signatures block_hash in
    let () = root_hash_check_keys root_hash_update storage block_hash in

    {
      current_block_hash = block_hash;
      current_block_height = block_height;
      current_state_hash = state_hash;
      current_handles_hash = handles_hash;
      current_validators = validators;
    }

(* bridge between root_hash and vault *)
let vault_add_handles_hash (handles_hash: blake2b) (storage: vault_storage) =
  let { known_handles_hash; used_handles; vault } = storage in
  let known_handles_hash = Big_map.add handles_hash () known_handles_hash in
  {
    known_handles_hash = known_handles_hash;
    used_handles = used_handles;
    vault = vault;
  }