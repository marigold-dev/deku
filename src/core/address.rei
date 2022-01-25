open Crypto;

[@deriving (ord, yojson)]
type t;

module Implicit: {
  let matches_key: (Key.t, Key_hash.t) => bool;

  let of_key: Key.t => Key_hash.t;
  let make: unit => (Secret.t, Key_hash.t);
};

module Originated: {
  [@deriving (ord, yojson)]
  type t;

  let to_contract_hash: t => Tezos.Contract_hash.t;
  let of_contract_hash: Tezos.Contract_hash.t => t;

  let to_string: t => string;
  let of_string: string => option(t);
};

let of_key_hash: Key_hash.t => t;
let to_key_hash: t => option(Key_hash.t);

let to_string: t => string;
let of_string: string => option(t);
