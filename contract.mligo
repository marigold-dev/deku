type validator = {
  key: key;
}

type validators = validator list

type storage = {
  current_validators: validators;
  current_state_hash: bytes;
  current_block_hash: bytes;
}

type signatures = signature option list

type action = {
  validators: validators;
  state_hash: bytes;
  block_payload_hash: bytes;
  (* sha256(state_root_hash + block_payload_hash) *)
  block_hash: bytes;
  (* sign(block_hash) *)
  signatures: signatures;
}

let check_hash
  (state_hash: bytes)
  (block_payload_hash: bytes)
  (block_hash: bytes) =
    let expected_block_hash =
        Crypto.sha256 (Bytes.concat state_hash block_payload_hash) in
    if block_hash = expected_block_hash
    then ()
    else failwith "invalid block hash"

let rec check_signatures
  (validators, signatures, block_hash, remaining:
   validators * signatures * bytes * int) : unit =
    match (remaining, validators, signatures) with
    (* already signed *)
    | (0, _, _) -> ()
    | (_, [], []) -> failwith "not enough sig"
    | (_, (v_hd :: v_tl), (sig_hd :: sig_tl)) ->
      (match sig_hd with
      | Some signature ->
        if Crypto.check v_hd.key signature block_hash
        then check_signatures (v_tl, sig_tl, block_hash, (remaining - 1))
        else failwith "bad sig"
      | None -> check_signatures (v_tl, sig_tl, block_hash, remaining))
    | (_, _, _) -> failwith "validators and sigs have diff size"

let check_storage_signatures
  (storage: storage)
  (block_hash: bytes)
  (signatures: signatures) =
    let validators_length = (int (List.length storage.current_validators)) in
    let required_validators = (validators_length * 2) / 3 in
    check_signatures (
      storage.current_validators,
      signatures,
      block_hash,
      required_validators
    )

let main (action, storage : action * storage) =
  let validators = action.validators in
  let state_hash = action.state_hash in
  let block_payload_hash = action.block_payload_hash in
  let block_hash = action.block_hash in
  let signatures = action.signatures in

  let () = check_hash state_hash block_payload_hash block_hash in
  let () = check_storage_signatures storage block_hash signatures in
  let storage = {
    current_validators = validators;
    current_state_hash = state_hash;
    current_block_hash = block_hash;
  } in
  (([] : operation list), storage)
