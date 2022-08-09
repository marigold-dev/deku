open Deku_concepts

type signer = Signer of { identity : Identity.t }
type t = signer

let make ~identity = Signer { identity }

let sign ~block signer =
  (* TODO: not double sign? *)
  let (Signer { identity }) = signer in
  let signature = Block.sign ~identity block in
  signature
