(* Constructing a large deku state to test state hashing is too awkward. The only parts of the Deku state that can be large are the ledger and the contract storage. Both are hash maps, so this is a good substitute.
   This is an easy way to test the speed of a bigmap -> yojson -> BLAKE2B function.
*)
open Helpers
open Crypto

(* Big map has ints as keys and small maps as values
   small maps have ints as keys and strings as values *)
module Map = Map.Make_with_yojson (struct
  type t = int [@@deriving yojson]

  let compare = Int.compare
end)

let small_map index =
  let rec go index map =
    if index = 0 then
      map
    else
      go (index - 1) (Map.add index (BLAKE2B.hash @@ Int.to_string index) map)
  in
  go index Map.empty

let big_map index small_map =
  let rec go index map =
    if index = 0 then
      map
    else
      go (index - 1) (Map.add index small_map map) in
  go index Map.empty

let generate_maps bmap_size smap_size =
  let smap = small_map smap_size in
  big_map bmap_size smap

(* hashes the json of a map of size bmap_size, with values that are each maps of size smap_size *)
let hash_the_map bmap_size smap_size =
  let map = generate_maps bmap_size smap_size in
  Map.to_yojson (Map.to_yojson BLAKE2B.to_yojson) map
  |> Yojson.Safe.to_string
  |> BLAKE2B.hash
