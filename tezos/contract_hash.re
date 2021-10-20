open Helpers;
open Crypto;
[@deriving eq]
type t = BLAKE2B_20.t;
let name = "Contract_hash";
let encoding = Data_encoding.(obj1(req(name, blake2b_20_encoding)));
let to_raw = BLAKE2B_20.to_raw_string;
let of_raw = BLAKE2B_20.of_raw_string;
let prefix = Crypto.Base58.Prefix.contract_hash;
let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
