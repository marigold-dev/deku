open Deku_protocol
open Deku_consensus
open Deku_concepts

type 'a post = Post of 'a
type get = Get

type _ endpoint = private
  | Blocks : Block.t post endpoint
  | Signatures : Verified_signature.t post endpoint
  | Operations : Operation.t post endpoint
  | Bootstrap : Bootstrap_signal.t post endpoint
  | Get_block_by_level : Level.t -> get endpoint

type 'a t = 'a endpoint
type ex = Ex : _ endpoint -> ex

val blocks : Block.t post endpoint
val signatures : Verified_signature.t post endpoint
val operations : Operation.t post endpoint
val bootstrap : Bootstrap_signal.t post endpoint

(* utils *)
val parse : path:string -> meth:Piaf.Method.t -> (ex, Internal_error.t) result
val to_string : _ endpoint -> string
