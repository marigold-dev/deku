open Crypto;

// TODO: this should probably not be here
module Michelson_v1_primitives = Michelson_v1_primitives;

type t;

let int: Z.t => t;
let nat: Z.t => t;
let bytes: bytes => t;
let pair: (t, t) => t;
let list: list(t) => t;
let key: Key.t => t;
let key_hash: Key_hash.t => t;
let address: Address.t => t;
let expr_encoding:
  Data_encoding.t(
    Tezos_micheline.Micheline.canonical(Michelson_v1_primitives.prim),
  );

let to_bytes: t => bytes;
