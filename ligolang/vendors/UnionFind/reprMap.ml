(* Map where the key is the repr *)

type ('item, 'a) t = {
  merge: 'a -> 'a -> 'a;
  map: ('item, 'a) PolyMap.t
}

let create ~cmp ~merge = {
  merge;
  map = PolyMap.create ~cmp;
}

let alias ?debug ~demoted_repr ~new_repr (m : (_,_) t) =
  { m with
    map = match PolyMap.find_opt demoted_repr m.map with
      | None -> m.map (* demoted_repr was not a key in the map, leaving it unchanged *)
      | Some other_value -> (* demoted_repr was a key in the map, we must either rename it or merge with the new_repr *)
        let map_without_demoted_repr = PolyMap.remove ?debug demoted_repr m.map in
          PolyMap.update
            new_repr
            (function
              (* new_repr was not a key in the map, be rename the key demoted_repr to new_repr *)
              | None -> Some other_value
              (* Both new_repr and demoted_repr were present as keys, remove the demoted_repr and merge their values *)
              | Some v -> Some (m.merge other_value v))
          map_without_demoted_repr }

let is_empty m = PolyMap.is_empty m.map

let add ?debug k v m = { m with map = PolyMap.add ?debug k v m.map }

let add_opt ?debug k v m = match PolyMap.find_opt k m.map with Some _ -> None | None -> Some (add ?debug k v m)

let monotonic_update k f m =
  { m with map = PolyMap.update k (function None -> Some (f None) | Some v -> Some (f (Some v))) m.map }

(* No removal, should be monotonic aside from merges due to aliasing *)
(* let remove k m = { m with map = PolyMap.remove k m.map } *)

(* find throws an exception, disabling it *)
(* let find k m = PolyMap.find k m.map *)

let find_opt k m = PolyMap.find_opt k m.map

let find_default k default m =
  let value, map = PolyMap.find_default k default m.map in
  value, { m with map }

let has_key k m = PolyMap.has_key k m.map

let bindings m = PolyMap.bindings m.map

let pp f g ppf m = PolyMap.pp f g ppf m.map