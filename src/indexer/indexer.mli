open Deku_consensus

type indexer
type t = indexer

val make : uri:Uri.t -> indexer Lwt.t
val save_block : block:Block.t -> timestamp:Timestamp.t -> indexer -> unit Lwt.t
val save_packet : packet:string -> timestamp:Timestamp.t -> indexer -> unit
