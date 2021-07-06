open Mirage_crypto_ec;

[@deriving (ord, yojson)]
type t;
[@deriving (ord, yojson)]
type key;

let genesis_secret: t;
let genesis_key: key;

let of_privkey: Ed25519.priv => t;
let to_privkey: t => Ed25519.priv;
let key_of_Ed25519pub: Ed25519.pub_ => key;
let key_to_Ed25519pub: key => Ed25519.pub_;

let to_key: t => key;
let make_pair: unit => (t, key);
let make_pubkey: unit => key;

let to_hex: string => string;
let of_hex: string => string;
