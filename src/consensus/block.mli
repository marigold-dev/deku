open Deku_crypto
open Deku_concepts
open Deku_protocol

type block = private
  | Block of {
      hash : Block_hash.t;
      author : Key_hash.t;
      (* TODO: why does it contain a level? *)
      level : Level.t;
      previous : Block_hash.t;
      payload : (Key.t * Signature.t * Operation.t) list;
    }

type t = block [@@deriving eq, ord, yojson]

val make :
  author:Key_hash.t ->
  level:Level.t ->
  previous:Block_hash.t ->
  payload:(Key.t * Signature.t * Operation.t) list ->
  block

val sign : identity:Identity.t -> block -> Verified_signature.t

module Set : Set.S with type elt = block
