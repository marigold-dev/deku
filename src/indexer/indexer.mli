open Deku_consensus

type indexer
type t = indexer

val make : uri:Uri.t -> indexer
val save_block : block:Block.t -> indexer -> indexer
val save_packet : packet:string -> timestamp:Timestamp.t -> indexer -> indexer
