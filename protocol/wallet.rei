open Crypto;

[@deriving (yojson, ord)]
type t = Key.t;

let of_key: Secret.t => t;

let genesis_key: Secret.t;
let genesis_wallet: t;

let to_string: t => string;
let of_string: string => option(t);
