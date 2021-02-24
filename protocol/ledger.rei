[@deriving yojson]
type t;
let empty: t;
let get_free: (Wallet.t, t) => Amount.t;
let get_frozen: (Wallet.t, t) => Amount.t;
let transfer:
  (~source: Wallet.t, ~destination: Wallet.t, ~amount: Amount.t, t) => t;

let freeze: (~wallet: Wallet.t, ~amount: Amount.t, t) => t;
let unfreeze: (~wallet: Wallet.t, ~amount: Amount.t, t) => t;

// on chain ops
let deposit: (~destination: Wallet.t, ~amount: Amount.t, t) => t;
let withdraw: (~source: Wallet.t, ~amount: Amount.t, t) => t;
