open Helpers;

[@deriving (yojson, ord)]
type validator = {
  address: Address.t,
  uri: Uri.t,
};

[@deriving yojson]
type t;

let current: t => option(validator);
let validators: t => list(validator);
let next: t => t;
let update_current: (Address.t, t) => t;

let empty: t;
let add: (validator, t) => t;
let remove: (validator, t) => t;
