open Helpers;
open Mirage_crypto_ec;

[@deriving (ord, yojson)]
type t = BLAKE2B.t;

let to_string = v => v |> to_yojson |> Yojson.Safe.to_string;

let of_address = pubkey => {
  let to_yojson = [%to_yojson: Address.t];
  pubkey |> to_yojson |> Yojson.Safe.to_string |> BLAKE2B.hash;
};
let pubkey_matches_wallet = (key, wallet) => {
  of_address(key) == wallet;
};
let get_pub_key = Ed25519.pub_of_priv;
