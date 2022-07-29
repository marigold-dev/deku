open Deku_concepts
open Deku_protocol
open Deku_consensus

type _ endpoint = private
  | Blocks : Block.t endpoint
  | Signatures : Verified_signature.t endpoint
  | Operations : Operation.t endpoint

type 'a t = 'a endpoint

val blocks : Block.t endpoint
val signatures : Verified_signature.t endpoint
val operations : Operation.t endpoint
val to_string : _ endpoint -> string
