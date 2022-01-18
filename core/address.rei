open Crypto;

[@deriving (ord, yojson)]
type t;

module Implicit: {
  [@deriving (eq, ord, yojson)]
  type t;

  let matches_key: (Key.t, t) => bool;

  let of_key: Key.t => t;
  let make: unit => (Secret.t, t);

  let to_key_hash: t => Key_hash.t;
  let of_key_hash: Key_hash.t => t;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Originated: {
  [@deriving (ord, yojson)]
  type t;

  let to_contract_hash: t => Tezos.Contract_hash.t;
  let of_contract_hash: Tezos.Contract_hash.t => t;

  let to_string: t => string;
  let of_string: string => option(t);
};

let of_implicit: Implicit.t => t;
let to_implicit: t => option(Implicit.t);

let to_string: t => string;
let of_string: string => option(t);
