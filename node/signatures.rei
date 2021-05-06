open Protocol;

[@deriving yojson]
type t;
// self_key should be the node public key
let make: (~self_key: Address.t, ~signatures_required: int) => t;
let is_self_signed: t => bool;
let is_signed: t => bool;
let set_signed: t => unit;
let add: (Multisig.signature, t) => unit;
let mem: (Multisig.signature, t) => bool;
