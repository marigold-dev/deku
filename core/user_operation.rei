open Crypto;

type initial_operation =
  | Transaction({
      destination: Address.t,
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
    sender: Address.t,
    initial_operation,
  };

let make: (~sender: Address.t, initial_operation) => t;
