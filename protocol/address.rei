open Crypto;

[@deriving (ord, yojson)]
type t;

let of_wallet: Wallet.t => t;
let pubkey_matches_wallet: (Wallet.t, t) => bool;
let make: unit => (Secret.t, t);

let to_key_hash: t => Key_hash.t;
let of_key_hash: Key_hash.t => t;

let to_string: t => string;
let of_string: string => option(t);
