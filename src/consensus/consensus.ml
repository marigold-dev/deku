open Deku_crypto
open Deku_concepts
open Block

type consensus =
  | Consensus of {
      validators : Validators.t;
      current_level : Level.t;
      current_block : Block_hash.t;
      last_block_author : Key_hash.t;
      last_block_update : Timestamp.t option;
    }

and t = consensus

let make ~validators ~block =
  let (Block { hash; level; author; _ }) = block in

  let current_block = hash in
  let current_level = level in
  let last_block_author = author in
  let last_block_update = None in
  Consensus
    {
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
      validators;
      current_level;
      current_block;
      last_block_author;
      last_block_update;
    }
