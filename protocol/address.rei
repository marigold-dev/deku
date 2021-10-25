open Crypto;

[@deriving (yojson, ord)]
type t = Ed25519.Key.t;

let of_key: Crypto.Secret.t => t;

let genesis_key: Crypto.Secret.t;
let genesis_address: t;

let make_pubkey: unit => t;

let to_string: t => string;
let of_string: string => option(t);
