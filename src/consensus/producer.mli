open Deku_concepts
open Deku_protocol

type producer
type t = producer [@@deriving yojson]

val empty : producer
val incoming_operation : operation:Operation.t -> producer -> producer

(* TODO: n log n *)
val clean : receipts:Receipt.t list -> producer -> producer
val produce : identity:Identity.t -> above:Block.t -> producer -> Block.block
