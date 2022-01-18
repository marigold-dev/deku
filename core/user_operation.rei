open Crypto;

type initial_operation =
  | Transaction({
      destination: Address.Implicit.t,
      amount: Amount.t,
      ticket: Ticket_id.t,
    })
  | Tezos_withdraw({
      owner: Tezos.Address.t,
      amount: Amount.t,
      ticket: Ticket_id.t,
    });

[@deriving (eq, ord, yojson)]
type t =
  pri {
    hash: BLAKE2B.t,
    source: Address.Implicit.t,
    initial_operation,
  };

let make: (~source: Address.Implicit.t, initial_operation) => t;
