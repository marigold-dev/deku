open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol

type block = private
  | Block of {
      (* TODO: I don't like that block carries signature *)
      key : Key.t;
      signature : Signature.t;
      hash : Block_hash.t;
      withdrawal_handles_hash : BLAKE2b.t;
      author : Key_hash.t;
      (* TODO: why does it contain a level? *)
      level : Level.t;
      previous : Block_hash.t;
      payload : string list;
      payload_hash : BLAKE2b.t;
      tezos_operations : Tezos_operation.t list;
    }

type t = block [@@deriving eq, ord, yojson]

val produce :
  identity:Identity.t ->
  level:Level.t ->
  previous:Block_hash.t ->
  payload:string list ->
  tezos_operations:Tezos_operation.t list ->
  withdrawal_handles_hash:BLAKE2b.t ->
  block

exception Failed_to_decode

val encode : block -> string list
val decode : string list -> block

(* TODO: tag signatures, should be more than sign of the hash of a block *)
val sign : identity:Identity.t -> block -> Verified_signature.t

module Set : Set.S with type elt = block

val pp : Format.formatter -> t -> unit
