open Deku_consensus
open Deku_concepts

type indexer
type t = indexer

val make : uri:Uri.t -> indexer Lwt.t
val save_block : block:Block.t -> timestamp:Timestamp.t -> indexer -> unit
val save_packet : packet:string -> timestamp:Timestamp.t -> indexer -> unit
val find_block_by_level : block_level:Level.t -> indexer -> Block.t option Lwt.t

val find_block_by_hash :
  block_hash:Block_hash.t -> indexer -> Block.t option Lwt.t
