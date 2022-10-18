open Deku_crypto
open Deku_concepts

exception Invalid_handshake
exception Invalid_message_size

module Connection : sig
  type connection
  type t = connection

  val owner : connection -> Key.t
  val read : connection -> Network_message.t
  val write : connection -> Network_message.t -> unit
end

module Client : sig
  exception Invalid_host

  val connect :
    identity:Identity.t ->
    net:#Eio.Net.t ->
    host:string ->
    port:int ->
    (Connection.t -> 'a) ->
    'a
end

module Server : sig
  val listen :
    identity:Identity.t ->
    net:#Eio.Net.t ->
    port:int ->
    on_error:(exn -> unit) ->
    (Connection.t -> unit) ->
    'a
end

val test : unit -> unit
