type blake2b = bytes
type level = nat
type validator = key_hash
type validators = validator list
type signatures = (key * signature) option list

type storage = {
  current_block_hash : blake2b;
  current_block_level : level;
  current_validators : validators;
}

type action = {
  block_level : level;
  block_content : blake2b;
  signatures : signatures;
}

let block_hash (block_level : level) (block_content : blake2b) =
  let hash_content = (block_level, block_content) in
  Crypto.blake2b (Bytes.pack hash_content)

let assert_block_level (current_block_level : level) (block_level : level) =
  assert (block_level > current_block_level)

let rec count_signatures (block_hash : blake2b) (total : nat)
    (validators : validators) (signatures : signatures) : nat =
  match (validators, signatures) with
  (* already signed *)
  | [], [] -> total
  | validator :: validators, Some (key, signature) :: signatures ->
      let () =
        assert (
          Crypto.hash_key key = validator
          && Crypto.check key signature block_hash)
      in
      count_signatures block_hash (total + 1n) validators signatures
  | _ :: validators, None :: signatures ->
      count_signatures block_hash total validators signatures
  | _ -> failwith "validators and signatures have different size"

let count_signatures (block_hash : blake2b) (validators : validators)
    (signatures : signatures) =
  count_signatures block_hash 0n validators signatures

let assert_signatures (validators : validators) (block_hash : blake2b)
    (signatures : signatures) =
  let validators_length = List.length validators in
  let required_signatures =
    match ediv validators_length 3n with
    | Some (majority, _remainder) -> majority + 1n
    | None -> failwith "divided by zero"
  in
  let signatures_length = count_signatures block_hash validators signatures in
  assert (signatures_length >= required_signatures)

let main ((action, storage) : action * storage) =
  let { block_level; block_content; signatures } = action in
  let { current_block_hash = _; current_block_level; current_validators } =
    storage
  in

  let block_hash = block_hash block_level block_content in

  let () = assert_block_level current_block_level block_level in
  let () = assert_signatures current_validators block_hash signatures in

  ( ([] : operation list),
    {
      current_block_hash = block_hash;
      current_block_level = block_level;
      current_validators = current_validators;
    } )
