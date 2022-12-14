open Deku_stdlib
open Deku_concepts
open Deku_consensus
open Deku_gossip

type storage
type t = storage

val make : worker:Parallel.Worker.t -> uri:Uri.t -> storage
val save_block : block:Block.t -> storage -> unit

val save_block_and_votes :
  level:Level.t -> network:Message.Network.t -> storage -> unit

val save_message : message:Message.Network.t -> storage -> unit

(* TODO: optimize store and load Message.Network.t *)
val find_block_by_level :
  level:Level.t -> storage -> Block.t option

val find_block_by_hash :
  block_hash:Block_hash.t -> storage -> Block.t option

val find_block_and_votes_by_level :
  level:Level.t -> storage -> Message.Network.t option
