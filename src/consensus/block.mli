open Deku_crypto
open Deku_concepts
open Deku_protocol

type block = private
  | Block of {
      (* TODO: I don't like that block carries signature *)
      signature : Signature.t;
      hash : Block_hash.t;
      (* TODO: author being Key.t is also weird *)
      author : Key.t;
      (* TODO: why does it contain a level? *)
      level : Level.t;
      previous : Block_hash.t;
      payload : (Key.t * Signature.t * Operation.t) list;
    }

type t = block [@@deriving eq, ord, yojson]

exception Invalid_signature

val produce :
  identity:Identity.t ->
  level:Level.t ->
  previous:Block_hash.t ->
  payload:(Key.t * Signature.t * Operation.t) list ->
  block

val sign : identity:Identity.t -> block -> Verified_signature.t

module Set : Set.S with type elt = block
