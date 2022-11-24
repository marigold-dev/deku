open Deku_crypto
open Deku_concepts
open Deku_tezos

type internal_operation =
  | Deposit of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Deku_tezos.Ticket_id.t;
    }

type tezos_operation = {
  hash : Tezos_operation_hash.t;
  operations : internal_operation list;
}

type t = tezos_operation

val make : Tezos_operation_hash.t -> internal_operation list -> t
val encoding : tezos_operation Data_encoding.t
