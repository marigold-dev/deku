open Deku_concepts
open Deku_protocol
open Deku_tezos

type producer_data = private
  | Producer_data of {
      operations : Operation.t Operation_hash.Map.t;
      (* TODO: should this be a set instead of map since
         we never do random access? *)
      tezos_operations : Tezos_operation.t Tezos_operation_hash.Map.t;
    }
[@@deriving yojson]

type producer
type t = producer

val make : identity:Identity.t -> default_block_size:int -> producer

val rehydrate :
  identity:Identity.t -> default_block_size:int -> producer_data -> producer

val dehydrate : producer -> producer_data
val incoming_operation : operation:Operation.t -> producer -> producer

val clean_data :
  receipts:Receipt.t list ->
  tezos_operations:Tezos_operation.t list ->
  producer_data ->
  producer_data

val incoming_tezos_operation :
  tezos_operation:Tezos_operation.t -> producer -> producer

(* TODO: n log n *)
val clean :
  receipts:Receipt.receipt list ->
  tezos_operations:Tezos_operation.tezos_operation list ->
  t ->
  t

val produce :
  parallel_map:
    ((Operation.operation -> string) -> Operation.operation list -> string list) ->
  above:Block.block ->
  withdrawal_handles_hash:Deku_crypto.BLAKE2b.BLAKE2b_256.hash ->
  t ->
  Block.block
