open Helpers;
open Mirage_crypto_ec;

[@deriving (ord, yojson)]
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
  wallet |> address_to_blake |> BLAKE2B_20.to_string;
