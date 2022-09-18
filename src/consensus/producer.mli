open Deku_concepts
open Deku_protocol

type producer
type t = producer

val make : identity:Identity.t -> default_block_size:int -> producer
val incoming_operation : operation:Operation.t -> producer -> producer

(* TODO: n log n *)
val clean : receipts:Receipt.t list -> producer -> producer

val produce :
  current:Timestamp.t -> state:State.t -> producer -> Block.block option
