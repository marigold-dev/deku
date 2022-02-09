open Crypto;

type internal_operation =
  | Tezos_deposit({
      destination: Tezos.Address.t,
      amount: Amount.t,
      ticket: Tezos.Ticket_id.t,
    });
type payload = {
  tezos_operation_hash: Tezos.Operation_hash.t,
  internal_operations: list(internal_operation),
};

[@deriving (eq, ord, yojson)]
type t =
  pri {
    hash: BLAKE2B.t,
    payload,
  };

let make: payload => t;
