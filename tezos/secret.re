open Helpers;
open Crypto;
[@deriving eq]
type t =
  | Ed25519(Ed25519.Secret.t);
let to_string =
  fun
  | Ed25519(secret) => Ed25519.Secret.to_string(secret);
let of_string = {
  let ed25519 = string => {
    let.some secret = Ed25519.Secret.of_string(string);
    Some(Ed25519(secret));
  };
  List.try_decode_list([ed25519]);
};
