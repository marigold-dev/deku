open Mirage_crypto_ec;
open Helpers;

[@deriving (ord, yojson)]
type t;

let of_address: Address.t => t;
let pubkey_matches_wallet: (Address.t, t) => bool;
let get_pub_key: Address.key => Address.t;
let make_wallet: unit => (Ed25519.priv, t);
let make_address: unit => t;
let address_to_blake: t => BLAKE2B_20.t;
let address_of_blake: BLAKE2B_20.t => t;
let address_to_string: t => string;
