open Deku_concepts

type signer
type t = signer

val make : identity:Identity.t -> signer
val sign : block:Block.t -> signer -> Verified_signature.t
