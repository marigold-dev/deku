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
  current_block_level: int;
  current_state_hash: blake2b;
  current_handles_hash: blake2b;
  current_validators: validators;
}

type signatures = signature option list

type root_hash_action = {
  block_level: int;
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
  (* FIXME: should be nat*)
  block_level: int;
  block_payload_hash: blake2b;
  state_hash: blake2b;
  handles_hash: blake2b;
}

let assert_msg ((message, condition): (string * bool)) =
  if not condition then
    failwith message

let root_hash_check_block_level
  (storage: root_hash_storage)
  (block_level: int) =
    assert_msg (
      "old block level",
      block_level > storage.current_block_level
    )

let root_hash_block_hash (root_hash_update: root_hash_action) =
  let block_hash_structure = {
    block_level = root_hash_update.block_level;
    block_payload_hash = root_hash_update.block_payload_hash;
    state_hash = root_hash_update.state_hash;
    handles_hash = root_hash_update.handles_hash;
  } in
  Crypto.blake2b (Bytes.pack block_hash_structure)

let rec root_hash_check_keys
  (validator_keys, validators, block_hash, remaining:
    validator_keys * validators * blake2b * int) : unit =
    match (validator_keys, validators) with
    | ([], []) ->
      if remaining > 0 then
        failwith "not enough keys"
    | ((Some validator_key :: vk_tl), (validator :: v_tl)) ->
      if (Crypto.hash_key validator_key) = validator then
        root_hash_check_keys (vk_tl, v_tl, block_hash, (remaining - 1))
      else failwith "validator_key does not match validator hash"
    | ((None :: vk_tl), (_ :: v_tl)) ->
      root_hash_check_keys (vk_tl, v_tl, block_hash, remaining)
    | (_, _) ->
      failwith "validator_keys and validators have different size"


let rec root_hash_check_signatures
  (validator_keys, signatures, block_hash, remaining:
    validator_keys * signatures * blake2b * int) : unit =
    match (validator_keys, signatures) with
    (* already signed *)
    | ([], []) ->
      (* TODO: this can be short circuited *)
      if remaining > 0 then
        failwith "not enough key-signature matches"
    | ((Some validator_key :: vk_tl), (Some signature :: sig_tl)) ->
      if Crypto.check validator_key signature block_hash
      then
        root_hash_check_signatures (vk_tl, sig_tl, block_hash, (remaining - 1))
      else failwith "bad signature"
    | ((_ :: vk_tl), (None :: sig_tl)) ->
      root_hash_check_signatures (vk_tl, sig_tl, block_hash, remaining)
    | ((None :: vk_tl), (_ :: sig_tl)) ->
      root_hash_check_signatures (vk_tl, sig_tl, block_hash, remaining)
    | (_, _) ->
      failwith "validators and signatures have different size"

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

let root_hash_main
  (root_hash_update: root_hash_action)
  (storage: root_hash_storage) =
    let block_hash = root_hash_block_hash root_hash_update in
    let block_level = root_hash_update.block_level in
    let state_hash = root_hash_update.state_hash in
    let handles_hash = root_hash_update.handles_hash in
    let validators = root_hash_update.validators in
    let signatures = root_hash_update.signatures in

    let () = root_hash_check_block_level storage block_level in
    let () = root_hash_check_signatures root_hash_update storage signatures block_hash in
    let () = root_hash_check_keys root_hash_update storage block_hash in

    {
      current_block_hash = block_hash;
      current_block_level = block_level;
      current_state_hash = state_hash;
      current_handles_hash = handles_hash;
      current_validators = validators;
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
  address: key_hash
}
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
      | None -> ((failwith "unreachable"): (bytes ticket * vault))) in
  let vault = Big_map.add (ticketer, data) ticket vault in
  {
    known_handles_hash = known_handles_hash;
    used_handles = used_handles;
    vault = vault;
  }

(* withdraw entrypoint *)
(*
  flow:
  validate handle hash
  validate if handle.id is used
  validate if the caller is the owner
  validate if the proof match the data provided

  mark handle.id as used
  split ticket in fragment and remaining
  store remaining
  send fragment to callback
*)

(* TODO: make me shorter *)
type vault_handle_structure = 
  | Tezos of {
              (* having the id is really important to change the hash,
                otherwise people would be able to craft handles using old proofs *)
              id: vault_handle_id;
              owner: address;
              amount: nat;
              ticketer: address;
              (* TODO: probably data_hash *)
              data: bytes;
            }
  | Deku of {
              (* having the id is really important to change the hash,
                otherwise people would be able to craft handles using old proofs *)
              id: vault_handle_id;
              owner: address;
              amount: nat;
              ticketer: string;
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

let pack_helper (vhs : vault_handle_structure) : bytes =
  match vhs with
    | Tezos content -> Bytes.pack content
    | Deku content -> Bytes.pack content

let handle_id (vhs : vault_handle_structure) : vault_handle_id =
  match vhs with
    | Tezos content -> content.id
    | Deku content -> content.id

let handle_owner (vhs : vault_handle_structure) : address =
  match vhs with
      | Tezos content -> content.owner
      | Deku content -> content.owner

let handle_amount (vhs : vault_handle_structure) : nat =
  match vhs with
      | Tezos content -> content.amount
      | Deku content -> content.amount

let handle_data (vhs : vault_handle_structure) : bytes =
  match vhs with
      | Tezos content -> content.data
      | Deku content -> content.data

let vault_check_handle_proof
  (proof: vault_handle_proof)
  (root: blake2b)
  (handle: vault_handle_structure) =
    let bit_is_set (bit: int) =
      Bitwise.and (Bitwise.shift_left 1n (abs bit)) (handle_id handle) <> 0n in
    let rec verify
      (bit, proof, parent: int * vault_handle_proof * blake2b): unit =
        match proof with
        | [] ->
          let calculated_hash = Crypto.blake2b (pack_helper handle) in
          assert_msg ("invalid handle data", parent = calculated_hash)
        | (left, right) :: tl ->
          let () =
            let calculated_hash = Crypto.blake2b (Bytes.concat left right) in
            assert_msg ("invalid proof hash", parent = calculated_hash) in
          verify (bit - 1, tl, (if bit_is_set bit then right else left)) in
    (* each bit in the handle_id indicates if we should go left or right in
       the proof, but the first bit to check is the MSB, so we use the length
       of the proof to know what is the MSB *)
    let most_significant_bit = int (List.length proof) - 1 in
    verify (most_significant_bit, proof, root)

let tezos_ticket_vault_withdraw (ticketer: address) (data: bytes) (amount: nat) (vault: vault) =
  let (old_ticket, vault) =
      match
        Big_map.get_and_update
          (ticketer, data)
          (None: bytes ticket option)
          vault
      with
      | (Some old_ticket, vault) -> (old_ticket, vault)
      | (None, _) -> (failwith "unreachable" : bytes ticket * vault) 
    in
  let ((_, (_, total)), old_ticket) = Tezos.read_ticket old_ticket in
  let (fragment, remaining) =
    match
      Tezos.split_ticket
        old_ticket
        (amount, abs (total - amount))
    with
    | Some (fragment, remaining) -> (fragment, remaining)
    | None -> (failwith "unreachable" : bytes ticket * bytes ticket) in
  (fragment, Big_map.add (ticketer, data) remaining vault)

let vault_withdraw (withdraw: vault_withdraw) (storage: vault_storage) =
  let handles_hash = withdraw.handles_hash in
  let handle = withdraw.handle in
  let proof = withdraw.proof in

  let { known_handles_hash; used_handles; vault } = storage in

  let () = assert_msg (
    "unknown handles hash",
    Big_map.mem handles_hash known_handles_hash
  ) in
  let () = assert_msg (
    "already used handle",
    not Big_map.mem (handle_id handle) used_handles
  ) in
  let () = assert_msg (
    "only the owner can withdraw a handle",
    (handle_owner handle) = Tezos.get_sender ()
  ) in
  let () = vault_check_handle_proof proof handles_hash handle in

  (* start transfer *)
  let _used_handles = Big_map.add (handle_id handle) () used_handles in

  match handle with
    | Tezos content -> 
      let (fragment, vault) = tezos_ticket_vault_withdraw content.ticketer content.data content.amount vault in
      let transaction = Tezos.transaction fragment 0tz withdraw.callback in
      (([transaction]: operation list), {
        known_handles_hash = known_handles_hash;
        used_handles = used_handles;
        vault = vault;
      })
    | Deku content ->
      let deku_addr_prefix = String.sub 0n 3n content.ticketer in
      if deku_addr_prefix <> "DK1" then failwith "Not a deku address"
      else
        let ticketer_bytes = Bytes.pack content.ticketer in
        let new_data = Bytes.concat ticketer_bytes content.data in
        let new_ticket = Tezos.create_ticket new_data content.amount in
        let transaction = Tezos.transaction new_ticket 0tz withdraw.callback in
      (([transaction]: operation list), {
        known_handles_hash = known_handles_hash;
        used_handles = used_handles;
        vault = vault;
      })

(* bridge between root_hash and vault *)
let vault_add_handles_hash (handles_hash: blake2b) (storage: vault_storage) =
  let { known_handles_hash; used_handles; vault } = storage in
  let known_handles_hash = Big_map.add handles_hash () known_handles_hash in
  {
    known_handles_hash = known_handles_hash;
    used_handles = used_handles;
    vault = vault;
  }

(* main contract *)
type storage = {
  root_hash: root_hash_storage;
  vault: vault_storage;
}
type action =
  | Update_root_hash of root_hash_action
  | Deposit of vault_deposit
  | Withdraw of vault_withdraw

let main (action, storage : action * storage)=
  let { root_hash; vault } = storage in
  match action with
  | Update_root_hash root_hash_update ->
    let root_hash = root_hash_main root_hash_update root_hash in
    let vault =
      vault_add_handles_hash
        root_hash.current_handles_hash
        vault in
    (([] : operation list), { root_hash = root_hash; vault = vault })
  | Deposit deposit ->
      let vault = vault_deposit deposit vault in
      (([] : operation list), { root_hash = root_hash; vault = vault; })
  | Withdraw withdraw ->
    let (operations, vault) = vault_withdraw withdraw vault in
    (operations, { root_hash = root_hash; vault = vault; })