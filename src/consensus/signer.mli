open Deku_concepts

type signer
type t = signer

val make :
  identity:Identity.t -> validators:Validators.t -> block:Block.t -> signer

val apply_block : current:Timestamp.t -> block:Block.t -> signer -> signer

val sign :
  current:Timestamp.t -> block:Block.t -> signer -> Verified_signature.t option
