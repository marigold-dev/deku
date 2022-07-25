open Deku_concepts

type signer
type t = signer

val make : identity:Identity.t -> signer

val sign :
  current:Timestamp.t ->
  consensus:Consensus.t ->
  block:Block.t ->
  signer ->
  Verified_signature.t option
