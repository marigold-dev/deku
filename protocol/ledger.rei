[@deriving yojson]
type t;
let empty: t;
let get_free: (Address.t, t) => Amount.t;
let get_frozen: (Address.t, t) => Amount.t;
let transfer:
  (~source: Address.t, ~destination: Address.t, ~amount: Amount.t, t) => t;

let freeze: (~address: Address.t, ~amount: Amount.t, t) => t;
let unfreeze: (~address: Address.t, ~amount: Amount.t, t) => t;

// on chain ops
let deposit: (~destination: Address.t, ~amount: Amount.t, t) => t;
let withdraw: (~source: Address.t, ~amount: Amount.t, t) => t;
