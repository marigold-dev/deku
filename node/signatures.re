open Helpers;
open Protocol;

module Signature_set =
  Set.Make_with_yojson({
    [@deriving (yojson, ord)]
    type t = Signature.t;
  });
// TODO: what if I think it is signed, but other nodes disagree on this?
type t = {
  self_key: Wallet.t,
  self_signed: bool,
  signed: bool,
  length: int,
  signatures: Signature_set.t,
};
let make = (~self_key) => {
  self_key,
  self_signed: false,
  signed: false,
  length: 0,
  signatures: Signature_set.empty,
};
let add = (~signatures_required, signature, t) =>
  // TODO: maybe do exists + add in a single pass?
  // TODO: do I need to hold the signatures after it is already signed?
  if (!Signature_set.mem(signature, t.signatures)) {
    // TODO: curious, does OCaml optimize this?
    let self_signed =
      t.self_signed || Signature.public_key(signature) == t.self_key;
    let signatures = Signature_set.add(signature, t.signatures);
    let length = t.length + 1;
    let signed = length >= signatures_required;
    let length = t.length + 1;
    {self_key: t.self_key, signed, self_signed, length, signatures};
  } else {
    t;
  };
let mem = (signature, t) => Signature_set.mem(signature, t.signatures);
let is_signed = t => t.signed;
let is_self_signed = t => t.self_signed;
let set_signed = t => {...t, signed: true};
let to_list = t => Signature_set.to_seq(t.signatures) |> List.of_seq;
