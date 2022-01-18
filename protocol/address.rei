open Crypto;

module Implicit: {
  [@deriving (ord, eq, yojson)]
  type t;

  let of_wallet: Wallet.t => t;
  let pubkey_matches_wallet: (Wallet.t, t) => bool;
  let make: unit => (Secret.t, t);

  let to_key_hash: t => Key_hash.t;
  let of_key_hash: Key_hash.t => t;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Originated: {
  [@deriving (ord, eq, yojson)]
  type t;

  let to_contract_hash: t => Contract_hash.t;
  let of_contract_hash: Contract_hash.t => t;

  let to_string: t => string;
  let of_string: string => option(t);
};

[@deriving (ord, eq, yojson)]
type t =
  | Implicit(Implicit.t)
  | Originated(Originated.t);

let of_implicit: Implicit.t => t;
let to_implicit: t => option(Implicit.t);

let of_originated: Originated.t => t;
let to_originated: t => option(Originated.t);

let to_string: t => string;
let of_string: string => option(t);
