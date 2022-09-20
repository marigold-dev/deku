open Deku_stdlib
open Deku_crypto
open Deku_chain

module Config : sig
  type config = private {
    secret : Secret.t; (* bootstrap *)
    validators : Key_hash.t list;
    nodes : Uri.t list;
  }

  type t = config [@@deriving yojson]

  val make :
    secret:Secret.t -> validators:Key_hash.t list -> nodes:Uri.t list -> config

  val read : folder:string -> config Lwt.t
  val write : folder:string -> config -> unit Lwt.t
end

module Chain : sig
  val read : folder:string -> Chain.t option Lwt.t
  val write : pool:Parallel.Pool.t -> folder:string -> Chain.t -> unit Lwt.t
end
