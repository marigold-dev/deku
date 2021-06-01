[@deriving (ord, yojson)]
type t;

// TODO: is this a good idea?
let of_address: Address.t => t;
let pubkey_matches_wallet: (Address.t, t) => bool;
let get_pub_key: Address.key => Address.t;
//let get_address: t => Address.t;

// for testing
let to_string: t => string;
