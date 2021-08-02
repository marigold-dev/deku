open Helpers;
open Mirage_crypto_ec;

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
    Cstruct.of_string(BLAKE2B.to_raw_string(hash))
    // TODO: isn't this double hashing? Seems weird
    |> Ed25519.sign(~key)
    |> Cstruct.to_string;
  let public_key = Ed25519.pub_of_priv(key);
  {signature, public_key};
};
let signature_to_b58check = t => {
  open Tezos_interop;
  let prefix = Base58.Prefix.ed25519_signature;
  let to_raw = t => t.signature;
  Base58.simple_encode(~prefix, ~to_raw, t);
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
    ~key=signature.public_key,
    ~msg=Cstruct.of_string(BLAKE2B.to_raw_string(hash)),
    Cstruct.of_string(signature.signature),
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
  let sign: (~key: Ed25519.priv, value) => t;
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
