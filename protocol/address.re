open Mirage_crypto_ec;

type key = Ed25519.priv;

// TODO: both functions are duplicated
let to_hex = str => {
  let `Hex(str) = Hex.of_string(str);
  str;
};
let of_hex = hex => Hex.to_string(`Hex(hex));
let key_to_yojson = t =>
  `String(Ed25519.priv_to_cstruct(t) |> Cstruct.to_string |> to_hex);
let key_of_yojson =
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

type t = Ed25519.pub_; // TODO: is okay to have this public
let compare = (a, b) =>
  Cstruct.compare(Ed25519.pub_to_cstruct(a), Ed25519.pub_to_cstruct(b));
let to_yojson = t =>
  `String(Ed25519.pub_to_cstruct(t) |> Cstruct.to_string |> to_hex);
let of_yojson =
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

let of_key = Ed25519.pub_of_priv;

let genesis_key = {|fdc6199df66d421df1496785497b3974b36862beac7c543a9c77b99ccf168f02|};
let genesis_key =
  switch (key_of_yojson(`String(genesis_key))) {
  | Ok(key) => key
  | Error(error) => failwith(error)
  };
let genesis_address = Ed25519.pub_of_priv(genesis_key);
