open Helpers;
open Protocol;

module Signature_set =
  Set_with_yojson_make({
    [@deriving (yojson, ord)]
    type t = Multisig.signature;
  });
// TODO: what if I think it is signed, but other nodes disagree on this?
[@deriving yojson]
type t = {
  self_key: Address.t,
  signatures_required: int,
  mutable self_signed: bool,
  mutable signed: bool,
  mutable length: int,
  mutable signatures: Signature_set.t,
};
let make = (~self_key, ~signatures_required) => {
  self_key,
  signatures_required,
  self_signed: false,
  signed: false,
  length: 0,
  signatures: Signature_set.empty,
};
let add = (signature, t) =>
  // TODO: maybe do exists + add in a single pass?
  // TODO: do I need to hold the signatures after it is already signed?
  if (!Signature_set.mem(signature, t.signatures)) {
    // TODO: curious, does OCaml optimize this?
    t.self_signed = t.self_signed || signature.key == t.self_key;
    t.signatures = Signature_set.add(signature, t.signatures);
    t.length = t.length + 1;
    t.signed = t.length >= t.signatures_required;
  };
let mem = (signature, t) => Signature_set.mem(signature, t.signatures);
let is_signed = t => t.signed;
let is_self_signed = t => t.self_signed;
let set_signed = t => t.signed = true;
