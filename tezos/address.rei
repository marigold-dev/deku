open Crypto;

[@deriving (eq, ord, yojson)]
type t =
  | Implicit(Key_hash.t)
  | Originated({
      contract: Contract_hash.t,
      entrypoint: option(string),
    });
let encoding: Data_encoding.t(t);
let to_string: t => string;
let of_string: string => option(t);
