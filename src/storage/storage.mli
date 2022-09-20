open Deku_stdlib
open Deku_chain

module Chain : sig
  val read : folder:string -> Chain.t option Lwt.t
  val write : pool:Parallel.Pool.t -> folder:string -> Chain.t -> unit Lwt.t
end
