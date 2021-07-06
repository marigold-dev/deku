open Mirage_crypto_ec;

type t = Ed25519.priv;
type key = Ed25519.pub_;

let of_privkey = priv => priv;
let to_privkey = priv => priv;

let key_of_Ed25519pub = pub_ => pub_;
let key_to_Ed25519pub = pub_ => pub_;

let to_key = Ed25519.pub_of_priv;

let make_pair = () => Ed25519.generate();
let make_pubkey = () => make_pair() |> snd;

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
let key_to_yojson = t =>
  `String(Ed25519.pub_to_cstruct(t) |> Cstruct.to_string |> to_hex);
let key_of_yojson =
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

let genesis_secret = {|fdc6199df66d421df1496785497b3974b36862beac7c543a9c77b99ccf168f02|};
let genesis_secret =
  switch (of_yojson(`String(genesis_secret))) {
  | Ok(key) => key
  | Error(error) => failwith(error)
  };

let genesis_key = to_key(genesis_secret);

let compare_key = (a, b) =>
  Cstruct.compare(Ed25519.pub_to_cstruct(a), Ed25519.pub_to_cstruct(b));

let compare = (a, b) =>
  Cstruct.compare(Ed25519.priv_to_cstruct(a), Ed25519.priv_to_cstruct(b));
