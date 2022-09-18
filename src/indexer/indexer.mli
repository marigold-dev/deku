open Deku_consensus
open Deku_gossip

type indexer
type t = indexer
type config = { save_messages : bool; save_blocks : bool }

val make : uri:Uri.t -> config:config -> indexer Lwt.t
val save_block : block:Block.t -> indexer -> unit
val save_message : message:Message.raw_message -> indexer -> unit
