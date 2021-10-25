open Helpers;
open Crypto;

[@deriving ord]
type t = BLAKE2B_20.t;

let of_address = pubkey => {
  let Key.Ed25519(pubkey) = pubkey;
  Ed25519.Key_hash.of_key(pubkey);
};
let pubkey_matches_wallet = (key, wallet) => {
  of_address(key) == wallet;
};
let get_pub_key = Address.of_key;

let make_address = () => {
  let (_key, pub_) = Ed25519.generate();
  of_address(Ed25519(pub_));
};
let make_wallet = () => {
  let (key, pub_) = Ed25519.generate();
  let wallet_address = of_address(Ed25519(pub_));

  (Secret.Ed25519(key), wallet_address);
};
let address_to_blake = t => t;
let address_of_blake = t => t;
let address_to_string = wallet =>
  Key_hash.(Ed25519(wallet |> address_to_blake) |> to_string);
let address_of_string = string =>
  switch (Key_hash.of_string(string)) {
  | Some(Ed25519(key)) => Some(key)
  | None => None
  };
let to_yojson = t => `String(address_to_string(t));
let of_yojson = json => {
  let.ok string = [%of_yojson: string](json);
  address_of_string(string) |> Option.to_result(~none="invalid address");
};
