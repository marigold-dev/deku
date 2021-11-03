open Crypto;

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
let balance: (Address.t, Ticket.t, t) => Amount.t;
let transfer:
  (~source: Address.t, ~destination: Address.t, Amount.t, Ticket.t, t) =>
  result(t, [> | `Not_enough_funds]);

// on chain ops
let deposit: (Address.t, Amount.t, Ticket.t, t) => t;
let withdraw:
  (
    ~source: Address.t,
    ~destination: Tezos_interop.Address.t,
    Amount.t,
    Ticket.t,
    t
  ) =>
  result((t, Handle.t), [> | `Not_enough_funds]);

let handles_find_proof: (Handle.t, t) => list((BLAKE2B.t, BLAKE2B.t));
// TODO: I don't like this API
let handles_find_proof_by_id:
  (int, t) => option((list((BLAKE2B.t, BLAKE2B.t)), Handle.t));
let handles_root_hash: t => BLAKE2B.t;
