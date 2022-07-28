open Deku_concepts

type signer
type t = signer

val make : identity:Identity.t -> signer

val try_to_sign :
  current:Timestamp.t ->
  consensus:Consensus.t ->
  block:Block.t ->
  signer ->
  Verified_signature.t option
