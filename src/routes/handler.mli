open Deku_chain

module Get_block_by_level : sig
  open Deku_concepts

  val handle :
    path:string -> chain:Chain.chain -> Level.level -> Piaf.Response.t
end
