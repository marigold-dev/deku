open Helpers;
open Mirage_crypto_ec;

[@deriving yojson]
type t;
let compare: (t, t) => int;
let public_key: t => Address.t;

let sign: (~key: Ed25519.priv, BLAKE2B.t) => t;
let verify: (~signature: t, BLAKE2B.t) => bool;

let signature_to_b58check: t => string;
let signature_to_b58check_by_address: t => (Address.t, string);
let signature_to_tezos_signature_by_address:
  t => (Address.t, Tezos_interop.Signature.t);

module type S = {
  type value;
  type signature = t;
  type t =
    pri {
      value,
      signature,
    };
  let sign: (~key: Ed25519.priv, value) => t;
  // TODO: maybe it should be something else?
  let verify: (~signature: signature, value) => bool;
};
module Make:
  (P: {
     type t;
     let hash: t => BLAKE2B.t;
   }) => S with type value = P.t;
