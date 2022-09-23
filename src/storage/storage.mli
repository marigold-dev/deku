open Deku_stdlib
open Deku_chain

module Chain : sig
  val read : folder:string -> Chain.chain_data option Lwt.t

  val write :
    pool:Parallel.Pool.t -> folder:string -> Chain.chain_data -> unit Lwt.t
end
