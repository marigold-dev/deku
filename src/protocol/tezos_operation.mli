open Deku_crypto
open Deku_concepts

type internal_operation =
  | Deposit of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Deku_tezos.Ticket_id.t;
    }

type tezos_operation = {
  hash : Deku_tezos.Operation_hash.t;
  operations : internal_operation list;
}

type t = tezos_operation [@@deriving yojson]

val make : Deku_tezos.Operation_hash.t -> internal_operation list -> t
