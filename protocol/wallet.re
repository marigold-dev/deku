open Helpers;
open Mirage_crypto_ec;

[@deriving (ord, yojson)]
type t = BLAKE2B_20.t;

let of_address = pubkey =>
  pubkey |> Address.to_ed25519 |>  Ed25519.pub_to_cstruct |> Cstruct.to_string |> BLAKE2B_20.hash;
let pubkey_matches_wallet = (key, wallet) => {
  of_address(key) == wallet;
};
let get_pub_key = Address.of_key;

let make_address = () => {
  let (_key, pub_) = Ed25519.generate();
  pub_ |> Address.of_ed25519
    |> of_address;
};
let make_wallet = () => {
  let (key, pub_) = Ed25519.generate();
  let wallet_address = pub_ |> Address.of_ed25519 |> of_address;

  (key, wallet_address);
};
let address_to_blake = t => t;
let address_of_blake = t => t;
let address_to_string = wallet =>
  wallet |> address_to_blake |> BLAKE2B_20.to_string;
