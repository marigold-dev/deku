open Protocol;

type t;
// self_key should be the node public key
let make: (~self_key: Wallet.t) => t;
let is_self_signed: t => bool;
let is_signed: t => bool;
let set_signed: t => t;
let add: (~signatures_required: int, Signature.t, t) => t;
let mem: (Signature.t, t) => bool;
let to_list: t => List.t(Signature.t);
