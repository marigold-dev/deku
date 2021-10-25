open Helpers;
open Crypto;

type t = Ed25519.Key.t; // TODO: is okay to have this public

let make_pubkey = () => {
  let (_priv, pub_) = Ed25519.generate();
  pub_;
};

let compare = Ed25519.Key.compare;
let to_string = t => Tezos_interop.Key.to_string(Ed25519(t));
let of_string = string => {
  let.some Ed25519(t) = Tezos_interop.Key.of_string(string);
  Some(t);
};

let to_yojson = t => `String(to_string(t));
let of_yojson = json => {
  let.ok string = [%of_yojson: string](json);
  of_string(string) |> Option.to_result(~none="failed to parse");
};

let of_key = secret =>
  switch (secret) {
  | Crypto.Secret.Ed25519(secret) => Crypto.Ed25519.Key.of_secret(secret)
  };

let genesis_key = {|edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd|};
let genesis_key =
  switch (Crypto.Secret.of_yojson(`String(genesis_key))) {
  | Ok(key) => key
  | Error(error) => failwith(error)
  };
let genesis_address = of_key(genesis_key);
