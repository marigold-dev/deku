[@deriving (ord, yojson)]
type t;

let of_address: Address.t => t;
let pubkey_matches_wallet: (Address.t, t) => bool;
let get_pub_key: Address.key => Address.t;
