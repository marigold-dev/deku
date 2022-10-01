module Connection : sig
  type connection
  type t = connection

  val read : connection -> Network_message.t
  val write : connection -> Network_message.t -> unit
end

module Client : sig
  exception Invalid_host

  val connect :
    net:#Eio.Net.t -> host:string -> port:int -> (Connection.t -> 'a) -> 'a
end

module Server : sig
  val listen :
    net:#Eio.Net.t ->
    port:int ->
    on_error:(exn -> unit) ->
    (Connection.t -> unit) ->
    'a
end

val test : unit -> unit
