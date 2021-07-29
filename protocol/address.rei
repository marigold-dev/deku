open Mirage_crypto_ec;

[@deriving yojson]
type key = Ed25519.priv;

[@deriving (yojson, ord)]
type t = Ed25519.pub_;

let of_key: key => t;

let genesis_key: key;
let genesis_address: t;

let make_pubkey: unit => t;
