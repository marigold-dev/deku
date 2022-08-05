open Deku_concepts

type verifier = private Verifier of { block_pool : Block_pool.t }
type t = verifier

val empty : verifier

type incoming_block_or_signature_result = private {
  apply : Block.t option;
  verifier : verifier;
}

val incoming_block :
  consensus:Consensus.t ->
  block:Block.t ->
  verifier ->
  incoming_block_or_signature_result

val incoming_signature :
  consensus:Consensus.t ->
  signature:Verified_signature.t ->
  verifier ->
  incoming_block_or_signature_result
