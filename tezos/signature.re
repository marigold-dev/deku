open Helpers;
open Crypto;

[@deriving eq]
type t =
  | Ed25519(Ed25519.Signature.t);
let sign = (secret, message) =>
  switch (secret) {
  | Secret.Ed25519(secret) => Ed25519(Ed25519.sign(secret, message))
  };
let check = (key, signature, message) =>
  switch (key, signature) {
  | (Key.Ed25519(key), Ed25519(signature)) =>
    Ed25519.verify(key, signature, message)
  };
let to_string =
  fun
  | Ed25519(sign) => Ed25519.Signature.to_string(sign);
let of_string = {
  let ed25519 = string => {
    let.some sign = Ed25519.Signature.of_string(string);
    Some(Ed25519(sign));
  };
  List.try_decode_list([ed25519]);
};
