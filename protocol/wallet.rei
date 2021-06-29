[@deriving (ord, yojson)]
type t;
[@deriving (ord, yojson)]
type pub_;


let genesis_key: t;

let pubkey_of_wallet: t => pub_;
let make_pair: unit => (t, pub_);

let to_hex: string => string;
let of_hex: string => string;

//let compare_pub: pub_ => pub_ => bool;