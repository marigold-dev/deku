open Deku_protocol

module type S = sig
  val init :
    unit ->
    (module Rapper_helper.CONNECTION) ->
    (unit, [> Caqti_error.call_or_retrieve ]) result Eio.Promise.t

  val on_block :
    state:Protocol.t ->
    operations:Operation.Initial.t list ->
    receipts:Receipt.t list ->
    (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t ->
    unit
end
