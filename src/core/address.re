open Crypto;
open Helpers;

[@deriving (eq, ord, yojson)]
type t =
  | Implicit(Key_hash.t)
  | Originated(Tezos.Contract_hash.t);

let of_key_hash = implicit => Implicit(implicit);
let to_key_hash = t =>
  switch (t) {
  | Implicit(implicit) => Some(implicit)
  | _ => None
  };

let to_contract_hash = t =>
  switch (t) {
  | Implicit(_key_hash) => None
  | Originated(contract_hash) => Some(contract_hash)
  };
let of_contract_hash = contract_hash => Originated(contract_hash);

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
