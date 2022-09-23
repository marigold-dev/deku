open Deku_concepts
open Deku_protocol
open Deku_crypto

type producer_data [@@deriving yojson]
type producer
type t = producer

val make : identity:Identity.t -> default_block_size:int -> producer

val rehydrate :
  identity:Identity.t -> default_block_size:int -> producer_data -> producer

val dehydrate : producer -> producer_data
val incoming_operation : operation:Operation.t -> producer -> producer

val incoming_tezos_operation :
  tezos_operation:Tezos_operation.t -> producer -> producer

(* TODO: n log n *)
val clean :
  receipts:Receipt.t list ->
  tezos_operations:Tezos_operation.t list ->
  producer ->
  producer

val produce :
  parallel_map:
    ((Operation.operation -> string) -> Operation.operation list -> string list) ->
  current:Timestamp.t ->
  consensus:Consensus.t ->
  withdrawal_handles_hash:BLAKE2b.t ->
  producer ->
  Block.block option
