open Deku_concepts
open Block
open Is_valid

type verifier =
  | Verifier of {
      validators : Validators.t;
      current_level : Level.t;
      current_block : Block_hash.t;
      block_pool : Block_pool.t;
    }

type t = verifier

let make ~validators ~block =
  let (Block { hash; level; _ }) = block in
  let current_level = level in
  let current_block = hash in
  let block_pool = Block_pool.empty in
  Verifier { validators; current_level; current_block; block_pool }

let apply_block ~block verifier =
  let (Verifier
        { validators; current_level = _; current_block = _; block_pool }) =
    verifier
  in
  let (Block { hash; level; _ }) = block in
  let current_level = level in
  let current_block = hash in
  Verifier { validators; current_level; current_block; block_pool }

type incoming_block_or_signature_result = {
  apply : Block.t option;
  verifier : verifier;
}

let is_signed_enough ~validators ~signatures =
  (* TODO: index signatures by signer *)
  let total_signatures =
    Verified_signature.Set.fold
      (fun signature counter ->
        let signer = Verified_signature.key_hash signature in
        match Validators.mem signer validators with
        | true -> counter + 1
        | false -> counter)
      signatures 0
  in

  let required_signatures =
    (* TODO: maybe should be 2/3 + 1? *)
    (* TODO: O(1) cardinal *)
    let validators = Validators.cardinal validators in
    let validators = Float.of_int validators in
    let required_signatures = Float.ceil validators *. (2.0 /. 3.0) in
    Float.to_int required_signatures
  in
  total_signatures >= required_signatures

let incoming_block_or_signature ~block_hash verifier =
  let (Verifier { validators; current_level; current_block; block_pool }) =
    verifier
  in
  match Block_pool.find_block ~block_hash block_pool with
  | Some block -> (
      match is_valid ~current_level ~current_block block with
      | true -> (
          let signatures = Block_pool.find_signatures ~block_hash block_pool in
          match is_signed_enough ~validators ~signatures with
          | true ->
              let verifier = apply_block ~block verifier in
              { apply = Some block; verifier }
          | false -> { apply = None; verifier })
      | false ->
          (* TODO: clean from Block_pool *)
          { apply = None; verifier })
  | None -> { apply = None; verifier }

let incoming_block ~block verifier =
  let (Verifier { validators; current_level; current_block; block_pool }) =
    verifier
  in
  let block_pool = Block_pool.append_block block block_pool in
  let verifier =
    Verifier { validators; current_block; current_level; block_pool }
  in

  let (Block { hash = block_hash; _ }) = block in
  incoming_block_or_signature ~block_hash verifier

let incoming_signature ~signature verifier =
  let (Verifier { validators; current_level; current_block; block_pool }) =
    verifier
  in
  let block_pool = Block_pool.append_signature signature block_pool in
  let verifier =
    Verifier { validators; current_block; current_level; block_pool }
  in

  let hash = Verified_signature.signed_hash signature in
  let hash = Block_hash.of_blake2b hash in
  incoming_block_or_signature ~block_hash:hash verifier
