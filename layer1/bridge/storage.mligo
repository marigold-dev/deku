#import "parameter.mligo" "Parameter"
#import "errors.mligo" "Errors"

module Types = struct
  type t = {
    root_hash: Parameter.Types.root_hash_storage;
    vault: Parameter.Types.vault_storage;
  }
end

module Utils = struct
  (* Data type definition for update_root_hash module *)
  type blake2b = Parameter.Types.blake2b
  type root_hash_action = Parameter.Types.root_hash_action
  type validator_keys = Parameter.Types.validator_keys
  type signatures = Parameter.Types.signatures
  type root_hash_storage = Parameter.Types.root_hash_storage
  type validators = Parameter.Types.validators
  type vault_storage = Parameter.Types.vault_storage

  (* Data type definition for deposit module *)
  type vault = Parameter.Types.vault
  type vault_deposit = Parameter.Types.vault_deposit
  type vault_storage = Parameter.Types.vault_storage

  (* Data type definition for withdraw module *)
  type blake2b = Parameter.Types.blake2b
  type vault = Parameter.Types.vault
  type vault_handle_proof = Parameter.Types.vault_handle_proof
  type vault_handle_structure = Parameter.Types.vault_handle_structure
  type vault_withdraw = Parameter.Types.vault_withdraw
  type vault_storage = Parameter.Types.vault_storage

  (* Update_root_hash related functions *)
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
      assert_with_error 
        (block_height > storage.current_block_height)
        Errors.old_block_hash

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

  (* Deposit related functions *) 
  let vault_deposit (deposit: vault_deposit) (storage: vault_storage) =
    let { known_handles_hash; used_handles; vault } = storage in
    let (content, ticket) =  Tezos.read_ticket deposit.ticket in
    let (ticketer, (data, _)) = content in
    let (ticket, vault) =
      match
        Big_map.get_and_update
          (ticketer, data)
          (None: bytes ticket option)
          vault
      with
      | (None, vault) -> (ticket, vault)
      | (Some old_ticket, vault) ->
        (match Tezos.join_tickets (old_ticket, ticket) with
        | Some ticket -> (ticket, vault)
        | None -> ((failwith Errors.unreachable): (bytes ticket * vault))) in
    let vault = Big_map.add (ticketer, data) ticket vault in
    {
      known_handles_hash = known_handles_hash;
      used_handles = used_handles;
      vault = vault;
    }

  (* Withdraw related functions *)
  let vault_check_handle_proof
    (proof: vault_handle_proof)
    (root: blake2b)
    (handle: vault_handle_structure) =
      let bit_is_set (bit: int) =
        Bitwise.and (Bitwise.shift_left 1n (abs bit)) handle.id <> 0n in
      let rec verify
        (bit, proof, parent: int * vault_handle_proof * blake2b): unit =
          match proof with
          | [] -> 
            let calculated_hash = Crypto.blake2b (Bytes.pack handle) in
            assert_with_error (parent = calculated_hash) Errors.invalid_handle_data
          | (left, right) :: tl ->
            let () = 
              let calculated_hash = Crypto.blake2b (Bytes.concat left right) in
              assert_with_error (parent = calculated_hash) Errors.invalid_proof_hash in
            verify (bit - 1, tl, (if bit_is_set bit then right else left)) in
      (* each bit in the handle_id indicates if we should go left or right in
          the proof, but the first bit to check is the MSB, so we use the length
          of the proof to know what is the MSB *)
      let most_significant_bit = int (List.length proof) - 1 in
      verify (most_significant_bit, proof, root)


  let vault_withdraw (withdraw: vault_withdraw) (storage: vault_storage) =
    let handles_hash = withdraw.handles_hash in
    let handle = withdraw.handle in
    let proof = withdraw.proof in

    let { known_handles_hash; used_handles; vault } = storage in

    let () = assert_with_error 
      (Big_map.mem handles_hash known_handles_hash)
      Errors.unknown_handle_hash
    in
    let () = assert_with_error 
      (not Big_map.mem handle.id used_handles)
      Errors.used_hash
    in
    let () = assert_with_error 
      (handle.owner = Tezos.sender)
      Errors.owner_withdraw_handle
    in
    let () = vault_check_handle_proof proof handles_hash handle in

    (* start transfer *)
    let used_handles = Big_map.add handle.id () used_handles in

    let (fragment, vault) =
      let (old_ticket, vault) =
        match 
          Big_map.get_and_update
            (handle.ticketer, handle.data)
            (None: bytes ticket option)
            vault
        with
        | (Some old_ticket, vault) -> (old_ticket, vault)
        | (None, _) -> (failwith Errors.unreachable : bytes ticket * vault) in
      let ((_, (_, total)), old_ticket) = Tezos.read_ticket old_ticket in
      let (fragment, remaining) =
        match
          Tezos.split_ticket
            old_ticket
            (handle.amount, abs (total - handle.amount))
        with
        | Some (fragment, remaining) -> (fragment, remaining)
        | None -> (failwith Errors.unreachable : bytes ticket * bytes ticket) in
      (fragment, Big_map.add (handle.ticketer, handle.data) remaining vault) in
    let transaction = Tezos.transaction fragment 0tz withdraw.callback in
    ([transaction], {
      known_handles_hash = known_handles_hash;
      used_handles = used_handles;
      vault = vault;
    })
end