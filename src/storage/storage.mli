open Deku_stdlib
open Deku_chain

module Chain : sig
  val read : file:string -> Chain.t option Lwt.t
  val write : pool:Parallel.Pool.t -> file:string -> Chain.t -> unit Lwt.t
end
