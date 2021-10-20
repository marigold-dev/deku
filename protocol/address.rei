[@deriving yojson]
type key = Tezos.Secret.t;

[@deriving (yojson, ord)]
type t = Tezos.Key.t;

let of_key: key => t;

let genesis_key: key;
let genesis_address: t;

let make_pubkey: unit => t;

let to_string: t => string;
let of_string: string => option(t);
