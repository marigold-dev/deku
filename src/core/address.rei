open Crypto;

[@deriving (eq, ord, yojson)]
// TODO: why is this type abstract?
type t;

let of_key_hash: Key_hash.t => t;
let to_key_hash: t => option(Key_hash.t);

/* TOOD: this should not be Tezos.Contract_hash.t */
let of_contract_hash: Tezos.Contract_hash.t => t;
let to_contract_hash: t => option(Tezos.Contract_hash.t);

let to_string: t => string;
let of_string: string => option(t);
