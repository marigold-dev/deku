open Crypto;

[@deriving yojson]
type key = Ed25519.Secret.t;

[@deriving (yojson, ord)]
type t = Ed25519.Key.t;

let of_key: key => t;

let genesis_key: key;
let genesis_address: t;

let make_pubkey: unit => t;

let to_string: t => string;
let of_string: string => option(t);
