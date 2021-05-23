open Helpers;
open Mirage_crypto_pk;

[@deriving yojson]
type t;
let compare: (t, t) => int;
let public_key: t => Address.t;

let verify: (~signature: t, SHA256.hash) => bool;

module type S = {
  type value;
  type signature = t;
  type t =
    pri {
      value,
      signature,
    };
  let sign: (~key: Rsa.priv, value) => t;
  // TODO: maybe it should be something else?
  let verify: (~signature: signature, value) => bool;
};
module Make:
  (P: {
     type t;
     let hash: t => SHA256.hash;
   }) => S with type value = P.t;

