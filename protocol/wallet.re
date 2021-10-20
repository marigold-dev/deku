open Helpers;
open Crypto;

[@deriving ord]
type t = Tezos.Key_hash.t;

let of_address = pubkey => Tezos.Key_hash.of_key(pubkey);
let pubkey_matches_wallet = (key, wallet) => {
  of_address(key) == wallet;
};
let get_pub_key = Tezos.Key.of_secret;

let make_address = () => {
  let (_key, pub_) = Ed25519.generate();
  Tezos.Key.Ed25519(pub_) |> of_address;
};
let make_wallet = () => {
  let (key, pub_) = Ed25519.generate();
  let wallet_address = of_address(Tezos.Key.Ed25519(pub_));

  (Tezos.Secret.Ed25519(key), wallet_address);
};
let of_address_hash = t => t;
let address_to_string = Tezos.Key_hash.to_string;
let address_of_string = string =>
  switch (Tezos.Key_hash.of_string(string)) {
  | Some(key) => Some(key)
  | None => None
  };
let to_yojson = t => `String(address_to_string(t));
let of_yojson = json => {
  let.ok string = [%of_yojson: string](json);
  address_of_string(string) |> Option.to_result(~none="invalid address");
};
