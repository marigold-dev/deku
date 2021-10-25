open Helpers;
open Crypto;

module Ed25519 = {
  include Ed25519;
  module Signature = {
    include Signature;
    let to_yojson = t => `String(to_string(t));
    let of_yojson = string => {
      let.ok string = [%of_yojson: string](string);
      of_string(string) |> Option.to_result(~none="Invalid_signature");
    };
  };
};
[@deriving (ord, yojson)]
type t = {
  // TODO: what is the name of a signature?
  signature: Ed25519.Signature.t,
  public_key: Address.t,
};
let public_key = t => t.public_key;
let sign = (~key, hash) => {
  // double hash because tezos always uses blake2b on CHECK_SIGNATURE
  let Secret.Ed25519(key) = key;
  let hash = BLAKE2B.to_raw_string(hash) |> BLAKE2B.hash;
  let signature =
    BLAKE2B.to_raw_string(hash)
    // TODO: isn't this double hashing? Seems weird
    |> Ed25519.sign(key);
  let public_key = Key.Ed25519(Ed25519.Key.of_secret(key));
  {signature, public_key};
};
let signature_to_b58check = t => Ed25519.Signature.to_string(t.signature);
let signature_to_b58check_by_address = t => {
  (t.public_key, signature_to_b58check(t));
};
let signature_to_tezos_signature_by_address = t => (
  t.public_key,
  Tezos_interop.Signature.Ed25519(t.signature),
);
let verify = (~signature, hash) => {
  let hash = BLAKE2B.to_raw_string(hash) |> BLAKE2B.hash;
  let Key.Ed25519(public_key) = signature.public_key;
  Ed25519.verify(
    public_key,
    signature.signature,
    BLAKE2B.to_raw_string(hash),
  );
};
module type S = {
  type value;
  type signature = t;
  type t =
    pri {
      value,
      signature,
    };
  let sign: (~key: Secret.t, value) => t;
  let verify: (~signature: signature, value) => bool;
};
module Make = (P: {
                 type t;
                 let hash: t => BLAKE2B.t;
               }) => {
  type value = P.t;
  type signature = t;
  type t = {
    value,
    signature,
  };
  let sign = (~key, value) => {
    // TODO: maybe reuse this hash?
    let signature = P.hash(value) |> sign(~key);
    {value, signature};
  };
  // TODO: pretty sure this should be reused
  let verify = (~signature, value) => P.hash(value) |> verify(~signature);
};
