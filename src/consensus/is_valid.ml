open Deku_concepts
open Block

let is_expected_level ~current_level block =
  let (Block { level; _ }) = block in
  Level.(equal (next current_level) level)

let is_expected_previous ~current_block block =
  let (Block { previous; _ }) = block in
  Block_hash.equal current_block previous

let is_valid ~current_level ~current_block block =
  is_expected_level ~current_level block
  && is_expected_previous ~current_block block
