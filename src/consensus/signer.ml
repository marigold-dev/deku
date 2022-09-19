open Deku_concepts
open Consensus
open Block

type signer = Signer of { identity : Identity.t }
type t = signer

let make ~identity = Signer { identity }

let is_signable ~current ~consensus block =
  let (Block { author; _ }) = block in
  is_valid ~block consensus && is_expected_author ~current ~author consensus

let try_to_sign ~current ~consensus ~block signer =
  let (Signer { identity }) = signer in
  match is_signable ~current ~consensus block with
  | true ->
      let signature = Block.sign ~identity block in
      Some signature
  | false -> None
