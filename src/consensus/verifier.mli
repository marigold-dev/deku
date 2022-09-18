open Deku_concepts
open Deku_crypto

type verifier
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

val find_signatures :
  block_hash:Block_hash.t -> verifier -> Verified_signature.t Key_hash.Map.t

(* FIXME: try with Protocol instead *)
val current_withdrawal_hash :
  block_hash:Block_hash.t -> verifier -> (BLAKE2b.t, unit) Result.t
