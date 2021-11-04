open Crypto;
open Key_hash;

[@deriving (eq, ord)]
type t = Key_hash.t;

let of_key = of_key;
let matches_key = (key, t) => equal(of_key(key), t);

let make = () => {
  let (key, pub_) = Ed25519.generate();
  let wallet_address = of_key(Ed25519(pub_));

  (Secret.Ed25519(key), wallet_address);
};
let to_key_hash = t => t;
let of_key_hash = t => t;

let to_string = to_string;
let of_string = of_string;

let to_yojson = to_yojson;
let of_yojson = of_yojson;
