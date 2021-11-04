open Crypto;

type t = Key.t;

let compare = Key.compare;
let to_string = Key.to_string;
let of_string = Key.of_string;
let to_yojson = Key.to_yojson;
let of_yojson = Key.of_yojson;

let of_key = secret =>
  switch (secret) {
  | Secret.Ed25519(secret) => Key.Ed25519(Ed25519.Key.of_secret(secret))
  | Secret.Secp256k1(secret) =>
    Key.Secp256k1(Secp256k1.Key.of_secret(secret))
  };

let genesis_key = {|edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd|};
let genesis_key =
  switch (Secret.of_yojson(`String(genesis_key))) {
  | Ok(key) => key
  | Error(error) => failwith(error)
  };
let genesis_address = of_key(genesis_key);
