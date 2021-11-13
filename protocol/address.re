open Crypto;
open Key_hash;

[@deriving ord]
type t = Key_hash.t;

let of_wallet = of_key;
let pubkey_matches_wallet = (key, t) => equal(of_key(key), t);

let make = () => {
  let (key, pub_) = Ed25519.generate();
  let wallet_address = of_wallet(Ed25519(pub_));

  (Secret.Ed25519(key), wallet_address);
};
let to_key_hash = t => t;
let of_key_hash = t => t;

let to_string = to_string;
let of_string = of_string;

let to_yojson = to_yojson;
let of_yojson = of_yojson;
