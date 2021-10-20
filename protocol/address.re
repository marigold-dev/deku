open Helpers;
open Crypto;

type key = Tezos.Secret.t;

let key_to_yojson = key => `String(Tezos.Secret.to_string(key));
let key_of_yojson = json => {
  let.ok string = [%of_yojson: string](json);
  Tezos.Secret.of_string(string) |> Option.to_result(~none="failed to parse");
};

type t = Tezos.Key.t; // TODO: is okay to have this public

let make_pubkey = () => {
  let (_priv, pub_) = Ed25519.generate();
  Tezos.Key.Ed25519(pub_);
};

let compare = Tezos.Key.compare;
let to_string = t => Tezos.Key.to_string(t);
let of_string = Tezos.Key.of_string;

let to_yojson = t => `String(to_string(t));
let of_yojson = json => {
  let.ok string = [%of_yojson: string](json);
  of_string(string) |> Option.to_result(~none="failed to parse");
};

let of_key = Tezos.Key.of_secret;

let genesis_key = {|edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd|};
let genesis_key =
  switch (key_of_yojson(`String(genesis_key))) {
  | Ok(key) => key
  | Error(error) => failwith(error)
  };
let genesis_address = of_key(genesis_key);
