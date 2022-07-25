open Deku_crypto
open Deku_concepts
open Deku_protocol

exception Invalid_signature

type block = private
  | Block of {
      (* TODO: I don't like that block carries signature *)
      key : Key.t;
      signature : Signature.t;
      hash : Block_hash.t;
      author : Key_hash.t;
      (* TODO: why does it contain a level? *)
      level : Level.t;
      previous : Block_hash.t;
      payload : string list;
    }

type t = block [@@deriving eq, ord, yojson]

val produce :
  identity:Identity.t ->
  level:Level.t ->
  previous:Block_hash.t ->
  operations:Operation.t list ->
  block

(* TODO: tag signatures, should be more than sign of the hash of a block *)
val sign : identity:Identity.t -> block -> Verified_signature.t

module Set : Set.S with type elt = block
