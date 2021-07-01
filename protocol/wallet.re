open Mirage_crypto_ec;

type t = Ed25519.priv;
type pub_ = Ed25519.pub_;

let wallet_of_privkey = priv => priv; 
let wallet_to_privkey = priv => priv; 

let pub_of_Ed25519pub = pub_ => pub_; 
let pub_to_Ed25519pub = pub_ => pub_; 

let pubkey_of_wallet = Ed25519.pub_of_priv;

let make_pair = () => Ed25519.generate();


let to_hex = str => {
  let `Hex(str) = Hex.of_string(str);
  str;
};
let of_hex = hex => Hex.to_string(`Hex(hex));

let to_yojson = t =>
  `String(Ed25519.priv_to_cstruct(t) |> Cstruct.to_string |> to_hex);
let of_yojson =
  fun
  | `String(key) =>
    // TODO: this raises an exception
    try(
      of_hex(key)
      |> Cstruct.of_string
      |> Ed25519.priv_of_cstruct
      |> Result.map_error(Format.asprintf("%a", Mirage_crypto_ec.pp_error))
    ) {
    | _ => Error("failed to parse")
    }
  | _ => Error("invalid type");
let pub_to_yojson = t =>
  `String(Ed25519.pub_to_cstruct(t) |> Cstruct.to_string |> to_hex);
let pub_of_yojson =
  fun
  | `String(key) =>
    try(
      of_hex(key)
      |> Cstruct.of_string
      |> Ed25519.pub_of_cstruct
      |> Result.map_error(Format.asprintf("%a", Mirage_crypto_ec.pp_error))
    ) {
    | _ => Error("failed to parse")
    }
  | _ => Error("invalid type");

let genesis_key = {|fdc6199df66d421df1496785497b3974b36862beac7c543a9c77b99ccf168f02|};
let genesis_key =
  switch (of_yojson(`String(genesis_key))) {
  | Ok(key) => key
  | Error(error) => failwith(error)
  };


let compare_pub = (a, b) =>
  Cstruct.compare(Ed25519.pub_to_cstruct(a), Ed25519.pub_to_cstruct(b));

let compare = (a, b) =>
  Cstruct.compare(Ed25519.priv_to_cstruct(a), Ed25519.priv_to_cstruct(b));
