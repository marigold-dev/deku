open Crypto;

[@deriving (yojson, ord)]
type validator = {address: Wallet.t};

[@deriving yojson]
type t;

let current: t => option(validator);
/* TODO: is to_list ok? It looses data(current) */
let to_list: t => list(validator);
let length: t => int;

// TODO: this function needs a better name
/** [after_current(n, t)] cycle validators starting after the current one
  best: O(1)
  worst: O(n) */
let after_current: (int, t) => option(validator);
let update_current: (Wallet.t, t) => t;

let empty: t;
let add: (validator, t) => t;
let remove: (validator, t) => t;

let hash: t => BLAKE2B.t;
