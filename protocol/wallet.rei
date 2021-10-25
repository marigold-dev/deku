open Crypto;

[@deriving (ord, yojson)]
type t;

let of_address: Address.t => t;
let pubkey_matches_wallet: (Address.t, t) => bool;
let make_wallet: unit => (Secret.t, t);

let to_key_hash: t => Key_hash.t;
let of_key_hash: Key_hash.t => t;

let to_string: t => string;
let of_string: string => option(t);
