open Deku_crypto
open Deku_concepts
open Block
open Is_valid

type signer =
  | Signer of {
      identity : Identity.t;
      validators : Validators.t;
      current_level : Level.t;
      current_block : Block_hash.t;
      last_block_author : Key_hash.t;
      last_block_update : Timestamp.t option;
    }

type t = signer

let make ~identity ~validators ~block =
  let (Block { hash; level; author; _ }) = block in
  Signer
    {
      identity;
      validators;
      current_level = level;
      current_block = hash;
      last_block_author = author;
      last_block_update = None;
    }

let apply_block ~current ~block signer =
  let (Block { hash; level; author; _ }) = block in
  let (Signer { identity; validators; _ }) = signer in

  let current_block = hash in
  let current_level = level in
  let last_block_author = author in
  let last_block_update = Some current in
  Signer
    {
      identity;
      validators;
      current_block;
      current_level;
      last_block_author;
      last_block_update;
    }

let is_expected_author ~current ~validators ~last_block_author
    ~last_block_update block =
  let (Block { author; _ }) = block in
  match last_block_update with
  | Some last_block_update -> (
      let skip = Timestamp.timeouts_since ~current ~since:last_block_update in
      let expected_author =
        Validators.skip ~after:last_block_author ~skip validators
      in
      match expected_author with
      | Some expected_author -> Key_hash.equal expected_author author
      | None -> false)
  | None -> false

let sign ~current ~block signer =
  let (Signer
        {
          identity;
          current_level;
          current_block;
          validators;
          last_block_author;
          last_block_update;
        }) =
    signer
  in
  match
    is_valid ~current_level ~current_block block
    && is_expected_author ~current ~validators ~last_block_author
         ~last_block_update block
  with
  | true ->
      let signature = Block.sign ~identity block in
      Some signature
  | false -> None
