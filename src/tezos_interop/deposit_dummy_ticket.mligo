type storage = address * bytes ticket list

type parameter =
  Dummy_deposit_to_deku
| Dummy_withdraw_from_deku 

type return = operation list * storage


// Consensus contract stuff
type blake2b = bytes

(* store hash *)
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
  known_handles_hash: vault_known_handles_hash_set;
  used_handles: vault_used_handle_set;
  vault: vault;
}

(* deposit entrypoint *)
type vault_deposit = {
  ticket: vault_ticket;
  (* WARNING: deposit address should be a valid sidechain address *)
  address: address
}

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



type consensus_contract_action =
  | Update_root_hash of root_hash_action
  | Deposit of vault_deposit
  | Withdraw of vault_withdraw


// Our stuff



// Two entrypoints

let withdraw (store : storage) : return = failwith "unimplemented"
let deposit (addr, tickets : storage) : return = 
 let contract : consensus_contract_action contract = Tezos.get_contract_with_error addr "contract not found" in
 let ticket = Tezos.create_ticket (Bytes.pack "test") 1n in
 let deposit = {
    ticket=ticket;
    (* WARNING: deposit address should be a valid sidechain address *)
    address=Tezos.source
  } in
  ([Tezos.transaction (Deposit deposit) 0tez contract], (addr, tickets))

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
   
let main (action, store : parameter * storage) : return =
  match action with
   Dummy_deposit_to_deku -> deposit store
 | Dummy_withdraw_from_deku -> withdraw store
