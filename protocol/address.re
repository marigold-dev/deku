open Helpers;

// TODO: both functions are duplicated
[@deriving yojson]
type t = BLAKE2B.t;

let of_pubkey = pubkey => {
  let to_yojson = [%to_yojson: Wallet.pub_];
  pubkey |> to_yojson |> Yojson.Safe.to_string |> BLAKE2B.hash;
};
let of_wallet = privkey => {
  let pubkey = Wallet.pubkey_of_wallet(privkey);
  of_pubkey(pubkey);
};

let address_matches_pubkey = (address, pubkey) => {
  of_pubkey(pubkey) == address;
};

let make_wallet = () => {
  let (key, pub_) = Wallet.make_pair();
  let address = of_pubkey(pub_);

  (key, address);
};

let address_to_blake = t => t;
let address_of_blake = t => t;

let address_to_string = address =>
  address |> address_to_blake |> BLAKE2B.to_string;

let make_address = () => {
  snd(make_wallet());
};

let genesis_address = of_wallet(Wallet.genesis_key);

let compare = (a, b) =>
  String.compare(BLAKE2B.to_string(a), BLAKE2B.to_string(b));