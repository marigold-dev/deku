open Deku_concepts
open Deku_protocol
open Deku_consensus

type _ endpoint =
  | Blocks : Block.t endpoint
  | Signatures : Verified_signature.t endpoint
  | Operations : Operation.t endpoint

type 'a t = 'a endpoint
type ex = Ex : _ endpoint -> ex

let blocks = Blocks
let signatures = Signatures
let operations = Operations

let of_string path =
  match path with
  | "/blocks" -> Some (Ex Blocks)
  | "/signatures" -> Some (Ex Signatures)
  | "/operations" -> Some (Ex Operations)
  | _ -> None

let to_string (type a) (endpoint : a endpoint) =
  match endpoint with
  | Blocks -> "/blocks"
  | Signatures -> "/signatures"
  | Operations -> "/operations"
