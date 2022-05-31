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

type t = { identity : identity } [@@deriving yojson]

let make ~identity = { identity }
