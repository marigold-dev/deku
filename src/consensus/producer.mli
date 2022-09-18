open Deku_concepts
open Deku_protocol

type producer
type t = producer

val make : identity:Identity.t -> producer
val incoming_operation : operation:Operation.t -> producer -> producer

(* TODO: n log n *)
val clean : receipts:Receipt.t list -> producer -> producer

val produce :
  current:Timestamp.t -> consensus:Consensus.t -> producer -> Block.block option
