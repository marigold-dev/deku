open Deku_crypto
open Deku_chain

module Config : sig
  type config = private {
    secret : Secret.t; (* bootstrap *)
    validators : Key_hash.t list;
    nodes : (string * int) list;
  }

  type t = config [@@deriving yojson]

  val make :
    secret:Secret.t ->
    validators:Key_hash.t list ->
    nodes:(string * int) list ->
    config

  val read : env:Eio.Stdenv.t -> folder:string -> config
  val write : env:Eio.Stdenv.t -> folder:string -> config -> unit
end

module Chain : sig
  val read : env:Eio.Stdenv.t -> folder:string -> Chain.t option
  val write : env:Eio.Stdenv.t -> folder:string -> Chain.t -> unit
end
