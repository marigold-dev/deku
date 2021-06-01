(* store hash *)
type validator = key
type validators = validator list

type storage = {
  (* TODO: is having current_block_hash even useful? *)
  (* consensus proof *)
  current_block_hash: bytes;
  current_block_height: int;
  current_state_hash: bytes;
  current_validators: validators;
}

type signatures = signature option list

(* Root_hash_update *)

(* TODO: this is a bad name *)
type root_hash_update = {
  block_hash: bytes;
  block_height: int;
  block_payload_hash: bytes;

  state_hash: bytes;
  (* TODO: performance, can this blown up? *)
  validators: validators;

  signatures: signatures;
}

(* (pair (pair int bytes) (pair bytes validators)) *)
(* TODO: performance, put this structures in an optimized way *)
type block_hash_structure = {
  block_height: int;
  block_payload_hash: bytes;
  state_hash: bytes;
  validators_hash: bytes;
}

let assert_msg ((message, condition): (string * bool)) =
  if not condition then
    failwith message
  
let check_block_height (storage: storage) (block_height: int) =
  assert_msg (
    "old block height",
    block_height > storage.current_block_height
  )

let check_hash (root_hash_update: root_hash_update) =
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

let rec check_signatures
  (validators, signatures, block_hash, remaining:
   validators * signatures * bytes * int) : unit =
    match (validators, signatures) with
    (* already signed *)
    | ([], []) ->
      (* TODO: this can be short circuited *)
      if remaining > 0 then
        failwith "not enough signatures"
    | ((_ :: v_tl), (None :: sig_tl)) ->
      check_signatures (v_tl, sig_tl, block_hash, remaining)
    | ((validator :: v_tl), (Some signature :: sig_tl)) ->
      if Crypto.check validator signature block_hash
      then check_signatures (v_tl, sig_tl, block_hash, (remaining - 1))
      else failwith "bad signature"
    | (_, _) ->
      failwith "validators and signatures have different size"

let check_signatures
  (storage: storage)
  (signatures: signatures)
  (block_hash: bytes) =
    let validators_length = (int (List.length storage.current_validators)) in
    let required_validators = (validators_length * 2) / 3 in
    check_signatures (
      storage.current_validators,
      signatures,
      block_hash,
      required_validators
    )

let main (root_hash_update, storage : root_hash_update * storage) =
  let block_hash = root_hash_update.block_hash in
  let block_height = root_hash_update.block_height in
  let state_hash = root_hash_update.state_hash in
  let validators = root_hash_update.validators in
  let signatures = root_hash_update.signatures in

  let () = check_block_height storage block_height in
  let () = check_hash root_hash_update in
  let () = check_signatures storage signatures block_hash in

  let storage = {
    storage with
    current_block_hash = block_hash;
    current_block_height = block_height;
    current_state_hash = state_hash;
    current_validators = validators;
  } in
  (([] : operation list), storage)
