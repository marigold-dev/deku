open Crypto
open Helpers

type identity = {
  secret : Secret.t;
  key : Key.t;
  t : Key_hash.t;
  uri : Uri.t;
}
[@@deriving yojson]

let make_identity ~secret ~key ~uri =
  { secret; key; t = Key_hash.of_key key; uri }

type t = {
  identity : identity;
  minimum_block_delay : float;
}
[@@deriving yojson]

let make ~identity ~minimum_block_delay =
  if minimum_block_delay < 0. then
    failwith "Minimum block delay must be positive";
  { identity; minimum_block_delay }
