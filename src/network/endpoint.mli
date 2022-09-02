open Deku_concepts
open Deku_protocol
open Deku_consensus

type _ endpoint = private
  | Blocks : Block.t endpoint
  | Signatures : Verified_signature.t endpoint
  | Operations : Operation.t endpoint
  | Bootstrap : Bootstrap_signal.t endpoint

type 'a t = 'a endpoint
type ex = Ex : _ endpoint -> ex

val blocks : Block.t endpoint
val signatures : Verified_signature.t endpoint
val operations : Operation.t endpoint
val bootstrap : Bootstrap_signal.t endpoint

(* utils *)
val of_string : string -> ex option
val to_string : _ endpoint -> string
