open Crypto;
open Helpers;

module Implicit = {
  open Key_hash;

  let of_key = of_key;
  let matches_key = (key, t) => equal(of_key(key), t);

  let make = () => {
    let (key, pub_) = Ed25519.generate();
    let wallet_address = of_key(Ed25519(pub_));

    (Secret.Ed25519(key), wallet_address);
  };
};

module Originated = {
  include Tezos.Contract_hash;

  let to_contract_hash = t => t;
  let of_contract_hash = t => t;
};

[@deriving (eq, ord, yojson)]
type t =
  | Implicit(Key_hash.t)
  | Originated(Originated.t);

let of_key_hash = implicit => Implicit(implicit);
let to_key_hash = t =>
  switch (t) {
  | Implicit(implicit) => Some(implicit)
  | _ => None
  };

let to_string =
  fun
  | Implicit(implicit) => Key_hash.to_string(implicit)
  | Originated(contract_hash) =>
    Tezos.Contract_hash.to_string(contract_hash);

let of_string = {
  let implicit = string => {
    let.some key_hash = Key_hash.of_string(string);
    Some(Implicit(key_hash));
  };
  let contract = string => {
    let.some contract_hash = Tezos.Contract_hash.of_string(string);
    Some(Originated(contract_hash));
  };
  Encoding_helpers.parse_string_variant([implicit, contract]);
};
