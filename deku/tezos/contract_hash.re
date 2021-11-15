open Helpers;
open Crypto;

[@deriving (eq, ord)]
type t = BLAKE2B_20.t;

include Encoding_helpers.Make_b58({
  type nonrec t = t;
  let name = "Contract_hash";
  let title = "A contract ID";

  let size = BLAKE2B_20.size;
  let prefix = Base58.Prefix.contract_hash;

  let to_raw = BLAKE2B_20.to_raw_string;
  let of_raw = BLAKE2B_20.of_raw_string;
});

let (to_yojson, of_yojson) =
  Yojson_ext.with_yojson_string("contract_hash", to_string, of_string);
