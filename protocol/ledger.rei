open Helpers;

module Handle: {
  [@deriving yojson]
  type t =
    pri {
      hash: BLAKE2B.t,
      id: int,
      owner: Tezos_interop.Address.t,
      amount: Amount.t,
      ticket: Ticket.t,
    };
};

[@deriving yojson]
type t;
let empty: t;
let balance: (Wallet.t, Ticket.t, t) => Amount.t;
let transfer:
  (~source: Wallet.t, ~destination: Wallet.t, Amount.t, Ticket.t, t) =>
  result(t, [> | `Not_enough_funds]);

// on chain ops
let deposit: (Wallet.t, Amount.t, Ticket.t, t) => t;
let withdraw:
  (
    ~source: Wallet.t,
    ~destination: Tezos_interop.Address.t,
    Amount.t,
    Ticket.t,
    t
  ) =>
  result((t, Handle.t), [> | `Not_enough_funds]);

// TODO: I don't like this API
let handles_find_proof:
  (int, t) => option((list((BLAKE2B.t, BLAKE2B.t)), Handle.t));
let handles_root_hash: t => BLAKE2B.t;
