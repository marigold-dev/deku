open Deku_crypto
open Deku_concepts
open Block
open State

let is_expected_level ~current_level block =
  let (Block { level; _ }) = block in
  Level.(equal (next current_level) level)

let is_expected_previous ~current_block block =
  let (Block { previous; _ }) = block in
  Block_hash.equal current_block previous

let is_valid ~state block =
  let (State
        {
          validators = _;
          current_level;
          current_block;
          last_block_author = _;
          last_block_update = _;
        }) =
    state
  in
  is_expected_level ~current_level block
  && is_expected_previous ~current_block block

(* signability *)
let is_expected_author ~current_author block =
  let (Block { author; _ }) = block in
  Key_hash.equal current_author author

let is_signable ~current ~state block =
  let current_author = State.current_author ~current state in
  match current_author with
  | Some current_author ->
      is_valid ~state block && is_expected_author ~current_author block
  | None -> false

(* acceptability *)
let is_signed ~validators ~block_pool block =
  let (Block { hash = block_hash; _ }) = block in
  let signatures = Block_pool.find_signatures ~block_hash block_pool in
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

  (* TODO: make it strict > 2/3*)
  let required_signatures =
    (* TODO: maybe should be 2/3 + 1? *)
    (* TODO: O(1) cardinal *)
    let validators = Validators.cardinal validators in
    let validators = Float.of_int validators in
    let required_signatures = Float.ceil (validators *. (2.0 /. 3.0)) in
    Float.to_int required_signatures
  in
  total_signatures > required_signatures

let is_accepted ~state ~block_pool block =
  let (State
        {
          validators;
          current_level = _;
          current_block = _;
          last_block_author = _;
          last_block_update = _;
        }) =
    state
  in
  (* TODO: should also check if the producer is the correct one *)
  is_valid ~state block && is_signed ~validators ~block_pool block

let is_producer ~current ~state key_hash =
  let current_author = State.current_author ~current state in
  match current_author with
  | Some author -> Key_hash.equal author key_hash
  | None -> (* TODO: this is misleading, false may mean "not sure" *) false
