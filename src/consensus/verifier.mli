open Deku_concepts

type verifier
type t = verifier

type incoming_block_or_signature_result = private {
  apply : Block.t option;
  verifier : verifier;
}

val make : validators:Validators.t -> block:Block.t -> verifier

val incoming_block :
  block:Block.t -> verifier -> incoming_block_or_signature_result

val incoming_signature :
  signature:Verified_signature.t ->
  verifier ->
  incoming_block_or_signature_result
