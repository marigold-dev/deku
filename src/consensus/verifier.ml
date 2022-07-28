open Deku_concepts
open Consensus
open Block

type verifier = Verifier of { block_pool : Block_pool.t }
type t = verifier

let empty = Verifier { block_pool = Block_pool.empty }

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

let incoming_block_or_signature ~consensus ~block_hash verifier =
  let (Consensus { validators; _ }) = consensus in
  let (Verifier { block_pool }) = verifier in

  let apply =
    match Block_pool.find_block ~block_hash block_pool with
    | Some block -> (
        let signatures = Block_pool.find_signatures ~block_hash block_pool in
        match
          is_valid ~block consensus && is_signed_enough ~validators ~signatures
        with
        | true -> Some block
        | false ->
            (* TODO: clean from Block_pool if not valid  *)
            None)
    | None -> None
  in
  { apply; verifier }

let incoming_block ~consensus ~block verifier =
  let (Verifier { block_pool }) = verifier in
  let block_pool = Block_pool.append_block block block_pool in
  let verifier = Verifier { block_pool } in

  let (Block { hash = block_hash; _ }) = block in
  incoming_block_or_signature ~consensus ~block_hash verifier

let incoming_signature ~consensus ~signature verifier =
  let (Verifier { block_pool }) = verifier in
  let block_pool = Block_pool.append_signature signature block_pool in
  let verifier = Verifier { block_pool } in

  let hash = Verified_signature.signed_hash signature in
  let hash = Block_hash.of_blake2b hash in
  incoming_block_or_signature ~consensus ~block_hash:hash verifier
