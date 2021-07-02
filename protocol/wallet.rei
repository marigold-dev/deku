open Mirage_crypto_ec;

[@deriving (ord, yojson)]
type t;
[@deriving (ord, yojson)]
type pub_;

let genesis_key: t;

let wallet_of_privkey: Ed25519.priv => t;
let wallet_to_privkey: t => Ed25519.priv;
let pub_of_Ed25519pub: Ed25519.pub_ => pub_;
let pub_to_Ed25519pub: pub_ => Ed25519.pub_;

let pubkey_of_wallet: t => pub_;
let make_pair: unit => (t, pub_);

let to_hex: string => string;
let of_hex: string => string /*let compare_pub: pub_ => pub_ => bool*/;