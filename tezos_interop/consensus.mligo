type blake2b = bytes

(* store hash *)
type validator = key
type validators = validator list

(* Root_hash_update contract *)
type root_hash_storage = {
  (* TODO: is having current_block_hash even useful? *)
  (* consensus proof *)
  current_block_hash: blake2b;
  current_block_height: int;
  current_state_hash: blake2b;
  current_validators: validators;
}

type signatures = signature option list

type root_hash_action = {
  block_hash: blake2b;
  block_height: int;
  block_payload_hash: blake2b;

  state_hash: blake2b;
  (* TODO: performance, can this blown up? *)
  validators: validators;

  signatures: signatures;
}

(* (pair (pair int bytes) (pair bytes validators)) *)
(* TODO: performance, put this structures in an optimized way *)
type block_hash_structure = {
  block_height: int;
  block_payload_hash: blake2b;
  state_hash: blake2b;
  validators_hash: blake2b;
}

let assert_msg ((message, condition): (string * bool)) =
  if not condition then
    failwith message

let root_hash_check_block_height
  (storage: root_hash_storage)
  (block_height: int) =
    assert_msg (
      "old block height",
      block_height > storage.current_block_height
    )

let root_hash_check_hash (root_hash_update: root_hash_action) =
  let block_hash_structure = {
    block_height = root_hash_update.block_height;
    block_payload_hash = root_hash_update.block_payload_hash;
    state_hash = root_hash_update.state_hash;
    (* TODO: should we do pack of list? *)
    validators_hash = Crypto.blake2b (Bytes.pack root_hash_update.validators)
  } in
  let calculated_hash = Crypto.blake2b (Bytes.pack block_hash_structure) in
  assert_msg (
    "invalid block hash",
    root_hash_update.block_hash = calculated_hash
  )

let rec root_hash_check_signatures
  (validators, signatures, block_hash, remaining:
    validators * signatures * blake2b * int) : unit =
    match (validators, signatures) with
    (* already signed *)
    | ([], []) ->
      (* TODO: this can be short circuited *)
      if remaining > 0 then
        failwith "not enough signatures"
    | ((_ :: v_tl), (None :: sig_tl)) ->
      root_hash_check_signatures (v_tl, sig_tl, block_hash, remaining)
    | ((validator :: v_tl), (Some signature :: sig_tl)) ->
      if Crypto.check validator signature block_hash
      then
        root_hash_check_signatures (v_tl, sig_tl, block_hash, (remaining - 1))
      else failwith "bad signature"
    | (_, _) ->
      failwith "validators and signatures have different size"

let root_hash_check_signatures
  (storage: root_hash_storage)
  (signatures: signatures)
  (block_hash: blake2b) =
    let validators_length = (int (List.length storage.current_validators)) in
    let required_validators = (validators_length * 2) / 3 in
    root_hash_check_signatures (
      storage.current_validators,
      signatures,
      block_hash,
      required_validators
    )

let root_hash_main
  (root_hash_update: root_hash_action)
  (storage: root_hash_storage) =
    let block_hash = root_hash_update.block_hash in
    let block_height = root_hash_update.block_height in
    let state_hash = root_hash_update.state_hash in
    let validators = root_hash_update.validators in
    let signatures = root_hash_update.signatures in

    let () = root_hash_check_block_height storage block_height in
    let () = root_hash_check_hash root_hash_update in
    let () = root_hash_check_signatures storage signatures block_hash in

    {
      storage with
      current_block_hash = block_hash;
      current_block_height = block_height;
      current_state_hash = state_hash;
      current_validators = validators;
    }

(* vault contract *)
type vault_storage = (address * bytes, bytes ticket) big_map
type vault_deposit = {
  ticket: bytes ticket;
  (* WARNING: deposit address should be a valid sidechain address *)
  address: address
}
let vault_deposit (deposit: vault_deposit) (vault: vault_storage) =
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
      | None -> ((failwith "unreachable"): (bytes ticket * vault_storage))) in
  Big_map.add (ticketer, data) ticket vault

(* main contract *)
type storage = {
  root_hash: root_hash_storage;
  vault: vault_storage;
}
type action =
  | Update_root_hash of root_hash_action
  | Deposit of vault_deposit

let main (action, storage : action * storage) =
  let { root_hash; vault } = storage in
  match action with
  | Update_root_hash root_hash_update ->
    let root_hash = root_hash_main root_hash_update root_hash in
    (([] : operation list), { root_hash = root_hash; vault = vault })
  | Deposit deposit ->
      let vault = vault_deposit deposit vault in
      (([] : operation list), { root_hash = root_hash; vault = vault; })
