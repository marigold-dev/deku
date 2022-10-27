open Deku_concepts
open Deku_protocol

type producer
type t = producer [@@deriving yojson]

val encoding : producer Data_encoding.t
val empty : producer
val incoming_operation : operation:Operation.Signed.t -> producer -> producer

val incoming_tezos_operation :
  tezos_operation:Tezos_operation.t -> producer -> producer

(* TODO: n log n *)
val clean :
  receipts:Receipt.receipt list ->
  tezos_operations:Tezos_operation.tezos_operation list ->
  t ->
  t

val produce :
  identity:Identity.t ->
  default_block_size:int ->
  above:Block.block ->
  withdrawal_handles_hash:Deku_crypto.BLAKE2b.BLAKE2b_256.hash ->
  t ->
  Block.block
