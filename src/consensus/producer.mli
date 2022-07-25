open Deku_concepts
open Deku_protocol

type producer
type t = producer

val make : identity:Identity.t -> producer
val incoming_operation : operation:Operation.t -> producer -> producer

(* TODO: clean operations after applying *)
val produce :
  current_level:Level.t -> previous:Block_hash.t -> producer -> Block.t
