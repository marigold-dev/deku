open Mirage_crypto_ec;

[@deriving yojson]
type key = Tz1_pk (Ed25519.priv);
let key_of_ed25519: Ed25519.priv => key;
let key_to_ed25519: key => Ed25519.priv;

[@deriving (yojson, ord)]
type t = Tz1(Ed25519.pub_);

let of_ed25519: Ed25519.pub_ => t
let to_ed25519: t => Ed25519.pub_;

let of_key: key => t;

let genesis_key: key;
let genesis_address: t;

let make_pubkey: unit => t;
