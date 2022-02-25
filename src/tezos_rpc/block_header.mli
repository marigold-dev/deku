open Tezos

type t = {
  hash : Block_hash.t;
  level : int32;
  proto : int;
  predecessor : Block_hash.t;
}

val of_yojson : Yojson.Safe.t -> (t, string) result
