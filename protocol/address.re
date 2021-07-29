open Mirage_crypto_ec;

type key = Tz1_pk (Ed25519.priv);

let key_of_ed25519  = (ed25519_key) => Tz1_pk (ed25519_key)
  let key_to_ed25519 = fun | Tz1_pk (ed25519_key) => ed25519_key

// TODO: both functions are duplicated
let to_hex = str => {
  let `Hex(str) = Hex.of_string(str);
  str;
};
let of_hex = hex => Hex.to_string(`Hex(hex));
let key_to_yojson = t =>
  `String(t |> key_to_ed25519 |> Ed25519.priv_to_cstruct |> Cstruct.to_string |> to_hex);
let key_of_yojson =
  fun
  | `String(key) =>
    // TODO: this raises an exception
    try(
      of_hex(key)
      |> Cstruct.of_string
      |> Ed25519.priv_of_cstruct
      |> Result.map(key_of_ed25519)
      |> Result.map_error(Format.asprintf("%a", Mirage_crypto_ec.pp_error))
    ) {
    | _ => Error("failed to parse")
    }
  | _ => Error("invalid type");

type t = Tz1 (Ed25519.pub_); // TODO: is okay to have this public
let of_ed25519 = ed25519_pub => Tz1 (ed25519_pub)
let to_ed25519 = fun | Tz1 (ed25519_pub) => ed25519_pub

let make_pubkey = () => {
  let (_priv, pub_) = Ed25519.generate();
  pub_ |> of_ed25519;
};

let compare = (a, b) => switch (a, b) {
  | (Tz1(a), Tz1(b)) => Cstruct.compare(Ed25519.pub_to_cstruct(a), Ed25519.pub_to_cstruct(b))
}
  
let to_yojson = fun | Tz1 (pub_) =>
  `String(pub_ |> Ed25519.pub_to_cstruct |> Cstruct.to_string |> to_hex);

let of_yojson =
  fun
  | `String(key) =>
    try(
      of_hex(key)
      |> Cstruct.of_string
      |> Ed25519.pub_of_cstruct
      |> Result.map(of_ed25519)
      |> Result.map_error(Format.asprintf("%a", Mirage_crypto_ec.pp_error))
    ) {
    | _ => Error("failed to parse")
    }
  | _ => Error("invalid type");

let of_key  = fun | Tz1_pk(k) => k |> Ed25519.pub_of_priv |> of_ed25519;

let genesis_key = {|fdc6199df66d421df1496785497b3974b36862beac7c543a9c77b99ccf168f02|};
let genesis_key =
  switch ((`String(genesis_key)) |> key_of_yojson) {
  | Ok(key) => key 
  | Error(error) => failwith(error)
  };
let genesis_address = switch(genesis_key) { | Tz1_pk (genesis_key) => genesis_key |> Ed25519.pub_of_priv |> of_ed25519 }
