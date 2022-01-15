open Crypto;

module Withdrawal_handle: {
  [@deriving yojson]
  type t =
    pri {
      hash: BLAKE2B.t,
      id: int,
      owner: Tezos.Address.t,
      amount: Amount.t,
      ticket: Ticket_id.t,
    };
};

[@deriving yojson]
type t;
let empty: t;
let balance: (Address.Implicit.t, Ticket.t, t) => Amount.t;
let transfer:
  (
    ~source: Address.Implicit.t,
    ~destination: Address.Implicit.t,
    Amount.t,
    Ticket.t,
    t
  ) =>
  result(t, [> | `Not_enough_funds]);

// on chain ops
let redeem_ticket_handle:
  (
    ~destination: Address.Implicit.t,
    ~ticket_id: Ticket_id.t=?,
    Ticket_table.Handle.t,
    t
  ) =>
  result(t, [> | `Invalid_ticket]);
let deposit: (Address.Implicit.t, Amount.t, Ticket_id.t, t) => t;
let withdraw:
  (
    ~source: Address.Implicit.t,
    ~destination: Tezos.Address.t,
    Amount.t,
    Ticket.t,
    t
  ) =>
  result((t, Handle.t), [> | `Not_enough_funds]);

let handles_find_proof:
  (Withdrawal_handle.t, t) => list((BLAKE2B.t, BLAKE2B.t));
// TODO: I don't like this API
let handles_find_proof_by_id:
  (int, t) => option((list((BLAKE2B.t, BLAKE2B.t)), Withdrawal_handle.t));
let handles_root_hash: t => BLAKE2B.t;
