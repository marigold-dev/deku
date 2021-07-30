[@deriving yojson]
type t;
let empty: t;
let balance: (Wallet.t, Ticket.t, t) => Amount.t;
let transfer:
  (~source: Wallet.t, ~destination: Wallet.t, Amount.t, Ticket.t, t) =>
  result(t, [> | `Not_enough_funds]);

// on chain ops
let deposit: (Wallet.t, Amount.t, Ticket.t, t) => t;
