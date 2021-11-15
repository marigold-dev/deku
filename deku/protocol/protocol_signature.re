open Crypto;

[@deriving (ord, yojson)]
type t = {
  // TODO: what is the name of a signature?
  signature: Signature.t,
  public_key: Wallet.t,
};
let public_key = t => t.public_key;
let sign = (~key as secret, hash) => {
  let signature = Signature.sign(secret, hash);
  let public_key = Key.of_secret(secret);
  {signature, public_key};
};
let signature_to_b58check = t => Signature.to_string(t.signature);
let signature_to_b58check_by_address = t => {
  (t.public_key, signature_to_b58check(t));
};
let signature_to_signature_by_address = t => (t.public_key, t.signature);
let verify = (~signature, hash) =>
  Signature.verify(signature.public_key, signature.signature, hash);
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
