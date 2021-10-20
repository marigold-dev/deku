[@deriving (ord, yojson)]
type t;

let of_address: Address.t => t;
let pubkey_matches_wallet: (Address.t, t) => bool;
let get_pub_key: Address.key => Address.t;
let make_wallet: unit => (Tezos.Secret.t, t);
let make_address: unit => t;
let of_address_hash: Tezos.Key_hash.t => t;
let address_to_string: t => string;
