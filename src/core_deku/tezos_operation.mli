open Crypto

type internal_operation =
  | Tezos_deposit of {
      destination : Tezos.Address.t;
      amount : Amount.t;
      ticket : Tezos.Ticket_id.t;
    }

type payload = {
  tezos_operation_hash : Tezos.Operation_hash.t;
  internal_operations : internal_operation list;
}

type t = private {
  hash : BLAKE2B.t;
  payload : payload;
}
[@@deriving eq, ord, yojson]

val make : payload -> t
