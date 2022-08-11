open Deku_concepts
open Deku_protocol
open Deku_consensus

type _ endpoint =
  | Blocks : Block.t endpoint
  | Signatures : Verified_signature.t endpoint
  | Operations : Operation.t endpoint
  | Bootstrap : Bootstrap_signal.t endpoint
  | Withdraw_proof : Ledger.withdraw_proof endpoint

type 'a t = 'a endpoint
type ex = Ex : _ endpoint -> ex

let blocks = Blocks
let signatures = Signatures
let operations = Operations
let bootstrap = Bootstrap
let withdraw_proof = Withdraw_proof

let of_string path =
  match path with
  | "/blocks" -> Some (Ex Blocks)
  | "/signatures" -> Some (Ex Signatures)
  | "/operations" -> Some (Ex Operations)
  | "/bootstrap" -> Some (Ex Bootstrap)
  | "/withdraw_proof" -> Some (Ex Withdraw_proof)
  | _ -> None

let to_string (type a) (endpoint : a endpoint) =
  match endpoint with
  | Blocks -> "/blocks"
  | Signatures -> "/signatures"
  | Operations -> "/operations"
  | Bootstrap -> "/bootstrap"
  | Withdraw_proof -> "/proof"
