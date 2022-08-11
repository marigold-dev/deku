open Deku_concepts
open Deku_protocol

type producer
type t = producer

val make : identity:Identity.t -> producer
val incoming_operation : operation:Operation.t -> producer -> producer

val incoming_tezos_operation :
  tezos_operation:Tezos_operation.t -> producer -> producer

(* TODO: n log n *)
val clean :
  receipts:Receipt.t list ->
  tezos_operations:Tezos_operation.t list ->
  producer ->
  producer

val try_to_produce :
  parallel_map:
    ((Operation.operation -> string) -> Operation.operation list -> string list) ->
  current:Timestamp.t ->
  consensus:Consensus.t ->
  producer ->
  Block.t option
