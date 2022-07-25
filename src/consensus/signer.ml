open Deku_crypto
open Deku_concepts
open Consensus
open Block
open Is_valid

type signer = Signer of { identity : Identity.t }
type t = signer

let make ~identity = Signer { identity }

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

let sign ~current ~consensus ~block signer =
  let (Consensus
        {
          validators;
          current_level;
          current_block;
          last_block_author;
          last_block_update;
        }) =
    consensus
  in
  let (Signer { identity }) = signer in
  match
    is_valid ~current_level ~current_block block
    && is_expected_author ~current ~validators ~last_block_author
         ~last_block_update block
  with
  | true ->
      let signature = Block.sign ~identity block in
      Some signature
  | false -> None
