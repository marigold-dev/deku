open Deku_protocol

type indexer = private
  | Indexer of { pool : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t }

type t = indexer

val register_etl : (module Etl_intf.S) -> unit
val make : uri:Uri.t -> indexer

val on_block :
  state:Protocol.t ->
  operations:Operation.Initial.t list ->
  receipts:Receipt.t list ->
  indexer ->
  unit
