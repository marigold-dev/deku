open Deku_concepts
open Deku_consensus
open Deku_gossip

type connection = (module Rapper_helper.CONNECTION)
type pool = ((module Rapper_helper.CONNECTION), Caqti_error.t) Caqti_eio.Pool.t
type 'a result_promise = ('a, Caqti_error.t) result Eio.Promise.t

val create_blocks_table : unit -> connection -> unit result_promise
val create_block_and_votes_table : unit -> connection -> unit result_promise
val create_packets_table : unit -> connection -> unit result_promise

val insert_block :
  block:Block.t -> timestamp:Timestamp.t -> pool -> unit result_promise

val insert_block_and_votes :
  level:Level.t ->
  network:Message.Network.t ->
  timestamp:Timestamp.t ->
  pool ->
  unit result_promise

val insert_message :
  message:Message.Network.t ->
  timestamp:Timestamp.t ->
  pool ->
  unit result_promise

val find_block_by_level : level:Level.t -> pool -> string option result_promise

val find_block_by_hash :
  hash:Block_hash.t -> pool -> string option result_promise

val find_block_and_votes_by_level :
  level:Level.t -> pool -> Message.Network.t option result_promise
