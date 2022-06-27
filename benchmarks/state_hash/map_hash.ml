(* Constructing a large deku state to test state hashing is too awkward. The only
   parts of the Deku state that can be large are the ledger and the contract storage.
   Both are hash maps, so this is a good substitute. This is an easy way to test
   the speed of a bigmap -> json serialization -> BLAKE2B function.
*)

open Crypto

(* Big map has ints as keys and small maps as values
   small map have ints as keys and strings as values *)
module Bench_yojson = struct
  module Map = Maps.Map_yojson

  let inner_map index =
    let rec go index map =
      if index = 0 then
        map
      else
        go (index - 1) (Map.add index (BLAKE2B.hash @@ Int.to_string index) map)
    in
    go index Map.empty

  let outer_map index inner_map =
    let rec go index map =
      if index = 0 then
        map
      else
        go (index - 1) (Map.add index inner_map map) in
    go index Map.empty

  let generate_maps bmap_size smap_size =
    let smap = inner_map smap_size in
    outer_map bmap_size smap

  (* hashes the json of a map of size bmap_size, with values that are each maps of size smap_size *)
  let hash_map map =
    Map.to_yojson (Map.to_yojson BLAKE2B.to_yojson) map
    |> Yojson.Safe.to_string
    |> BLAKE2B.hash
end

module Bench_bin_prot = struct
  module Map = Maps.Outer_map
  module Encoding = Maps.Encoding

  let inner_map index =
    let rec go index map =
      if index = 0 then
        map
      else
        go (index - 1)
          (Map.add_exn ~key:index
             ~data:(BLAKE2B.hash @@ Int.to_string index)
             map) in
    go index Map.empty

  let outer_map index inner_map =
    let rec go index map =
      if index = 0 then
        map
      else
        go (index - 1) (Map.add_exn ~key:index ~data:inner_map map) in
    go index Map.empty

  let generate_maps bmap_size smap_size =
    let smap = inner_map smap_size in
    outer_map bmap_size smap

  (* hashes the json of a map of size bmap_size, with values that are each maps of size smap_size *)
  let hash_map map =
    Encoding.pack Map.bin_writer_t map |> Bytes.to_string |> BLAKE2B.hash
end
