open Tezos
type t = {
  hash : Block_hash.t;
  level : int32;
  proto : int;
  predecessor : Block_hash.t;
}
[@@deriving of_yojson { strict = false }]
