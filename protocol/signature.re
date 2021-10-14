open Helpers;
open Crypto;

[@deriving yojson]
type t = {
  // TODO: what is the name of a signature?
  signature: string,
  public_key: Address.t,
};
// TODO: is it safe to compare only the signature?
let compare = (a, b) => String.compare(a.signature, b.signature);
let public_key = t => t.public_key;
let sign = (~key, hash) => {
  // double hash because tezos always uses blake2b on CHECK_SIGNATURE
  let hash = BLAKE2B.to_raw_string(hash) |> BLAKE2B.hash;
  let signature =
    BLAKE2B.to_raw_string(hash)
    // TODO: isn't this double hashing? Seems weird
    |> Ed25519.sign(key);
  let public_key = Ed25519.Key.of_secret(key);
  {signature, public_key};
};
let signature_to_b58check = t => {
  let prefix = Crypto.Base58.Prefix.ed25519_signature;
  let to_raw = t => t.signature;
  Crypto.Base58.simple_encode(~prefix, ~to_raw, t);
};
let signature_to_b58check_by_address = t => {
  (t.public_key, signature_to_b58check(t));
};
let signature_to_tezos_signature_by_address = t => (
  t.public_key,
  Tezos_interop.Signature.of_raw_string(`Ed25519(t.signature)),
);
let verify = (~signature, hash) => {
  let hash = BLAKE2B.to_raw_string(hash) |> BLAKE2B.hash;
  Ed25519.verify(
    signature.public_key,
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
  let sign: (~key: Ed25519.Secret.t, value) => t;
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
