module Chain : sig
  open Deku_chain

  val read : env:Eio.Stdenv.t -> folder:string -> Chain.t option
  val write : env:Eio.Stdenv.t -> folder:string -> Chain.t -> unit
end
