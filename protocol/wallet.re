open Helpers;
open Mirage_crypto_ec;

[@deriving ord]
type t = BLAKE2B_20.t;

let of_address = pubkey =>
  Ed25519.pub_to_cstruct(pubkey) |> Cstruct.to_string |> BLAKE2B_20.hash;
let pubkey_matches_wallet = (key, wallet) => {
  of_address(key) == wallet;
};
let get_pub_key = Ed25519.pub_of_priv;

let make_address = () => {
  let (_key, pub_) = Ed25519.generate();
  pub_ |> of_address;
};
let make_wallet = () => {
  let (key, pub_) = Ed25519.generate();
  let wallet_address = of_address(pub_);

  (key, wallet_address);
};
let address_to_blake = t => t;
let address_of_blake = t => t;
let address_to_string = wallet =>
  Tezos_interop.Key_hash.(Ed25519(wallet |> address_to_blake) |> to_string);
let address_of_string = string =>
  switch (Tezos_interop.Key_hash.of_string(string)) {
  | Some(Ed25519(key)) => Some(key)
  | None => None
  };
let to_yojson = t => `String(address_to_string(t));
let of_yojson = json => {
  let.ok string = [%of_yojson: string](json);
  address_of_string(string) |> Option.to_result(~none="invalid address");
};
