open Crypto;

[@deriving yojson]
type t;

[@deriving yojson]
type receipt =
  | Receipt_tezos_withdraw(Ledger.Withdrawal_handle.t);

let empty: t;
let ledger: t => Ledger.t;
let hash: t => BLAKE2B.t;

let apply_user_operation: (t, User_operation.t) => (t, option(receipt));
let apply_tezos_operation: (t, Tezos_operation.t) => t;
