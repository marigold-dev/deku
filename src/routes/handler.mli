open Deku_chain
open Deku_indexer

module Get_block_by_level : sig
  open Deku_concepts

  val handle :
    path:string ->
    chain:Chain.chain ->
    indexer:Indexer.t ->
    Level.level ->
    Piaf.Response.t Lwt.t
end
