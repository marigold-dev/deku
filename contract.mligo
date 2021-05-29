type validator = key
type validators = validator list

type uri = bytes
type uri_map = (key, uri) map

type storage = {
  (* TODO: is having current_block_hash even useful? *)
  (* consensus proof *)
  current_block_hash: bytes;
  current_block_height: int;
  current_state_hash: bytes;
  current_validators: validators;

  (* node detection*)
  current_validators_uri: uri_map;
}

type signatures = signature option list

(* Root_hash_update *)

(* TODO: performance, make this incremental, it can blown up *)
(* TODO: this is a bad name *)
type root_hash_update = {
  block_hash: bytes;
  block_height: int;
  block_payload_hash: bytes;

  state_hash: bytes;
  (* TODO: performance, make this a diff, it can blown up *)
  validators: validators;

  signatures: signatures;
}

(* (pair (pair (pair int bytes) bytes) bytes) *)
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
    storage.current_block_height >= block_height
  )

let check_hash (root_hash_update: root_hash_update) =
  let block_hash_structure = {
    block_height = root_hash_update.block_height;
    block_payload_hash = root_hash_update.block_payload_hash;
    state_hash = root_hash_update.state_hash;
    (* TODO: should we do pack of list? *)
    validators_hash = Crypto.sha256 (Bytes.pack root_hash_update.validators)
  } in
  let calculated_hash = Crypto.sha256 (Bytes.pack block_hash_structure) in
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
      if not remaining = 0 then
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

let update_root_hash
  (storage: storage)
  (root_hash_update: root_hash_update) =
    let block_hash = root_hash_update.block_hash in
    let block_height = root_hash_update.block_height in
    let state_hash = root_hash_update.state_hash in
    let validators = root_hash_update.validators in
    let signatures = root_hash_update.signatures in

    let () = check_block_height storage block_height in
    let () = check_hash root_hash_update in
    let () = check_signatures storage signatures block_hash in

    {
      storage with
      current_block_hash = block_hash;
      current_block_height = block_height;
      current_state_hash = state_hash;
      current_validators = validators;
    }

(* Validator_uri_update *)
type validator_uri_update = {
  key: key;
  uri: uri;
  signature: signature;
}

let validators_list_to_set (validators: validators) =
  List.fold_left
    (fun ((set, validator): (validator set * validator)) ->
      Set.add validator set)
    (Set.empty: validator set)
    validators

let check_signature (validator_uri_update: validator_uri_update) =
  let key = validator_uri_update.key in
  let signature = validator_uri_update.signature in
  let uri = validator_uri_update.uri in
  assert_msg (
    "invalid signature",
    Crypto.check key signature uri
  )

let equal_keys (a: key) (b: key) =
    [%Michelson ({| { UNPAIR; COMPARE; EQ } |} : (key * key) -> bool)] (a, b)

let update_validator_uri
  (storage: storage)
  (validator_uri_update: validator_uri_update) =
    let validators = validators_list_to_set storage.current_validators in
    let () = check_signature validator_uri_update in
    let validators_uri =
      (* TODO: move this to it's own function *)
      Map.fold
        (* TODO: if I understand the worst case here is O(n log n)
          but by doing validators diff this cleanup logic could be removed *)
        (fun ((validators_uri, (key, current_uri)): (uri_map * (key * uri))) ->
          (* this will filter dead keys in the map of old validators *)
          if Set.mem key validators then
            let uri =
              if equal_keys key validator_uri_update.key then
                validator_uri_update.uri
              else
                current_uri in
            Map.add key uri validators_uri
          else
            validators_uri)
        storage.current_validators_uri
        (Map.empty : uri_map) in
    { storage with current_validators_uri = validators_uri }

type action =
  | Root_hash_update of root_hash_update
  | Validator_uri_update of validator_uri_update

let main (action, storage : action * storage) =
  let storage =
    match action with
    | Root_hash_update root_hash_update ->
      update_root_hash storage root_hash_update
    | Validator_uri_update validator_uri_update ->
      update_validator_uri storage validator_uri_update in
  (([] : operation list), storage)
