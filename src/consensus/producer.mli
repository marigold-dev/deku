open Deku_concepts
open Deku_protocol

type producer
type t = producer

val make : identity:Identity.t -> producer
val apply_block : block:Block.t -> producer -> producer

val incoming_operation :
  signature:Verified_signature.t ->
  operation:Operation.t ->
  producer ->
  producer

val produce :
  current_level:Level.t -> previous:Block_hash.t -> producer -> Block.t
