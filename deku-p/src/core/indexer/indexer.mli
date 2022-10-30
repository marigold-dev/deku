open Deku_protocol

type indexer
type t = indexer

val make : uri:Uri.t -> indexer

val on_block :
  state:Protocol.t ->
  operations:Operation.Initial.t list ->
  receipts:Receipt.t list ->
  indexer ->
  unit
