open Helpers;

[@deriving yojson]
type t;

let of_wallet: Wallet.t => t;
let of_pubkey: Wallet.key => t;

let make_wallet: unit => (Wallet.t, t);

let genesis_address: t;

let address_to_blake: t => BLAKE2B.t;
let address_of_blake: BLAKE2B.t => t;

let address_to_string: t => string;

let address_matches_pubkey: (t, Wallet.key) => bool;

let make_address: unit => t;

let compare: (t, t) => int;