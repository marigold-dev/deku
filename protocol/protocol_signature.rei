open Crypto;

[@deriving yojson]
type t;
let compare: (t, t) => int;
let public_key: t => Wallet.t;
let address: t => Address.t;

let sign: (~key: Secret.t, BLAKE2B.t) => t;
let verify: (~signature: t, BLAKE2B.t) => bool;

let signature_to_signature_by_address: t => (Address.t, Signature.t);

module type S = {
  type value;
  type signature = t;
  type t =
    pri {
      value,
      signature,
    };
  let sign: (~key: Secret.t, value) => t;
  // TODO: maybe it should be something else?
  let verify: (~signature: signature, value) => bool;
};
module Make:
  (P: {
     type t;
     let hash: t => BLAKE2B.t;
   }) => S with type value = P.t;
