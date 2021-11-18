(* Polymorphic maps *)

module RB = RedBlack

type ('key, 'value) t = {
  tree : ('key * 'value) RB.t;
  cmp  : 'key -> 'key -> int
}

type ('key, 'value) map = ('key, 'value) t

let create ~cmp = {tree = RB.empty; cmp}

let empty map = {tree = RB.empty; cmp=map.cmp}

let is_empty map = RB.is_empty map.tree

let add ?debug key value map =
  let cmp (k1,_) (k2,_) = map.cmp k1 k2 in
  {map with tree = RB.add ?debug ~cmp RB.New (key, value) map.tree}

let remove ?debug key map =
  let cmp k1 (k2,_) = map.cmp k1 k2 in
  {map with tree = RB.delete ?debug ~cmp key map.tree}

let find key map =
  let cmp k1 (k2,_) = map.cmp k1 k2 in
  try snd (RB.find ~cmp key map.tree) with
    Not_found -> raise Not_found

let find_opt key map =
  try Some (find key map) with Not_found -> None

let find_default key make_default_v map =
  try find key map, map with
    Not_found -> let v = make_default_v () in v, add key v map

let has_key key map =
  match find_opt key map with
    Some _ -> true
  | None -> false

let update key updater map =
  match updater (find_opt key map) with
  | None -> remove key map
  | Some v -> add key v map

type ('key, 'value) added = {map : ('key, 'value) t; duplicates : ('key * 'value) list; added : ('key * 'value) list}
let add_list elts map =
  let aux = fun {map ; duplicates ; added} ((key, value) as kv) ->
    if has_key key map
    then {map; duplicates = kv :: duplicates ; added}
    else {map = add key value map; duplicates; added = kv :: added} in
  List.fold_left aux {map; duplicates=[]; added = []} elts

let from_list ~cmp elts =
  match add_list elts (create ~cmp) with
    { map; duplicates = []; added = _ } -> Some map
  | _ -> None (* Refuse to create a map from a list with duplicates *)


let bindings map =
  RB.fold_dec (fun ~elt ~acc -> elt::acc) ~init:[] map.tree

let keys map = List.map fst (bindings map)
let values map = List.map snd (bindings map)

let get_compare set = set.cmp

let iter f map = RB.iter (fun (k,v) -> f k v) map.tree

let fold_inc f map = RB.fold_inc (fun ~elt:(k,v) -> f k v) map.tree

let pp f g ppf (map : ('a,'b) t) =
  Format.fprintf ppf "@[(%a)@]"
  (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ,@ ") (fun ppf (a,b) -> Format.fprintf ppf "(%a,%a)" f a g b)) (bindings map)
