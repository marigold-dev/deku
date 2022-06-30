module Types = struct
  type blake2b = bytes

  (* Store hash *)
  type validator = key_hash
  type validators = validator list
  type validator_key = key
  type validator_keys = validator_key option list

  (* Root_hash_update contract *)
  type root_hash_storage = {
    (* TODO: is having current_block_hash even useful? *)
    (* consensus proof *)
    current_block_hash: blake2b;
    current_block_height: int;
    current_state_hash: blake2b;
    current_handles_hash: blake2b;
    current_validators: validators;
  }

  type signatures = signature option list

  type root_hash_action = {
    block_height: int;
    block_payload_hash: blake2b;

    state_hash: blake2b;
    handles_hash: blake2b;
    (* TODO: performance, can this blown up? *)
    validators: validators;

    current_validator_keys: validator_keys;
    signatures: signatures;
  }

  (* (pair (pair int bytes) (pair bytes validators)) *)
  (* TODO: performance, put this structures in an optimized way *)
  type block_hash_structure = {
    block_height: int;
    block_payload_hash: blake2b;
    state_hash: blake2b;
    handles_hash: blake2b;
    validators_hash: blake2b;
  }

  (* vault contract *)
  type vault_ticket = bytes ticket
  type vault = (address * bytes, vault_ticket) big_map
  type vault_handle_id = nat
  type vault_used_handle_set = (vault_handle_id, unit) big_map
  type vault_known_handles_hash_set = (blake2b, unit) big_map
  type vault_storage = {
    known_handles_hash: (blake2b, unit) big_map;
    used_handles: vault_used_handle_set;
    vault: vault;
  }

  (* deposit entrypoint *)
  type vault_deposit = {
    ticket: vault_ticket;
    (* WARNING: deposit address should be a valid sidechain address *)
    address: address
  }

  (* TODO: this could be variable in size *)
  type vault_handle_structure = {
    (* having the id is really important to change the hash,
      otherwise people would be able to craft handles using old proofs *)
    id: vault_handle_id;
    owner: address;
    amount: nat;
    ticketer: address;
    (* TODO: probably data_hash *)
    data: bytes;
  }
  type vault_handle_proof = (blake2b * blake2b) list
  type vault_withdraw = {
    handles_hash: blake2b;
    handle: vault_handle_structure;
    proof: vault_handle_proof;
    callback: vault_ticket contract;
  }

  type t =
  | Update_root_hash of root_hash_action
  | Deposit of vault_deposit
  | Withdraw of vault_withdraw
end