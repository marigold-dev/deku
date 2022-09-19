open Deku_crypto
open Deku_concepts
open Block

type consensus =
  | Consensus of {
      bootstrap_key : Key.t;
      validators : Validators.t;
      current_level : Level.t;
      current_block : Block_hash.t;
      last_block_author : Key_hash.t;
      last_block_update : Timestamp.t option;
    }

and t = consensus

let make ~validators ~bootstrap_key =
  let (Block { hash; level; author; _ }) = Genesis.block in

  let current_block = hash in
  let current_level = level in
  let last_block_author = author in
  let last_block_update = None in
  Consensus
    {
      bootstrap_key;
      validators;
      current_level;
      current_block;
      last_block_author;
      last_block_update;
    }

let apply_block ~current ~block consensus =
  let (Block { hash; level; author; _ }) = block in
  let (Consensus
        {
          bootstrap_key;
          validators;
          current_level = _;
          current_block = _;
          last_block_author = _;
          last_block_update = _;
        }) =
    consensus
  in

  let current_block = hash in
  let current_level = level in
  let last_block_author = author in
  let last_block_update = Some current in
  Consensus
    {
      bootstrap_key;
      validators;
      current_level;
      current_block;
      last_block_author;
      last_block_update;
    }

let apply_bootstrap_signal ~bootstrap_signal ~current consensus =
  let (Bootstrap_signal.Bootstrap_signal
        { bootstrap_key = given_key; next_author; signature = _ }) =
    bootstrap_signal
  in
  let (Consensus
        {
          bootstrap_key;
          validators;
          current_level;
          current_block;
          last_block_author = _;
          last_block_update = _;
        }) =
    consensus
  in
  match Key.equal bootstrap_key given_key with
  | true ->
      Some
        (Consensus
           {
             bootstrap_key;
             validators;
             current_level;
             current_block;
             last_block_author = next_author;
             last_block_update = Some current;
           })
  | false -> None

(* judging *)
let is_expected_level ~current_level block =
  let (Block { level; _ }) = block in
  Level.(equal (next current_level) level)

let is_expected_previous ~current_block block =
  let (Block { previous; _ }) = block in
  Block_hash.equal current_block previous

let is_valid ~block consensus =
  let (Consensus { current_level; current_block; _ }) = consensus in
  is_expected_level ~current_level block
  && is_expected_previous ~current_block block

let expected_author ~current consensus =
  let (Consensus
        {
          bootstrap_key = _;
          validators;
          current_level = _;
          current_block = _;
          last_block_author;
          last_block_update;
        }) =
    consensus
  in
  match last_block_update with
  | Some last_block_update ->
      let skip = Timestamp.timeouts_since ~current ~since:last_block_update in
      Validators.skip ~after:last_block_author ~skip validators
  | None -> None

let is_expected_author ~current ~author consensus =
  match expected_author ~current consensus with
  | Some expected_author -> Key_hash.equal expected_author author
  | None -> false
