open Deku_concepts
open Deku_protocol
open Deku_consensus

type _ endpoint =
  | Blocks : Block.t endpoint
  | Signatures : Verified_signature.t endpoint
  | Operations : Operation.t endpoint

type 'a t = 'a endpoint

let blocks = Blocks
let signatures = Signatures
let operations = Operations

let to_string (type a) (endpoint : a endpoint) =
  match endpoint with
  | Blocks -> "/blocks"
  | Signatures -> "/signatures"
  | Operations -> "/operations"
