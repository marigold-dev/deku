open Helpers;
open Crypto;
[@deriving eq]
type t = BLAKE2B.t;

let prefix = Base58.Prefix.operation_hash;
let to_raw = BLAKE2B.to_raw_string;
let of_raw = BLAKE2B.of_raw_string;
let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
