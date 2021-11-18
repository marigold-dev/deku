(** Persistent implementation of the Union/Find algorithm with
    height-balanced forests and no path compression. *)

let equal compare i j = compare i j = 0

type height = int

(** Each equivalence class is implemented by a Catalan tree linked
    upwardly and otherwise is a link to another node. Those trees
    are height-balanced. The type [node] implements nodes in those
    trees. *)
type 'item node =
  Root of height
(** The value of [Root h] denotes the root of a tree, that is,
    the representative of the associated class.  The height [h]
    is that of the tree, so a tree reduced to its root alone has
    heigh 0. *)

| Link of 'item * height
(** If not a root, a node is a link to another node. Because the
    links are upward, that is, bottom-up, and we seek a purely
    functional implementation, we need to uncouple the nodes and
    the items here, so the first component of [Link] is an item,
    not a node. That is why the type [node] is not recursive,
    and called [node], not [tree]: to become a traversable tree,
    it needs to be complemented by the type [partition] below to
    associate items back to nodes. In order to follow a path
    upward in the tree until the root, we start from a link node
    giving us the next item, then find the node corresponding to
    the item thanks to [partition], and again until we arrive at
    the root.

    The height component is that of the source of the link, that
    is, [h] is the height of the node linking to the node [Link
    (j,h)], _not_ of [j], except when [equal i j]. *)

(* module ItemMap = Map.Make (Item) *)

type ('item, 'value) map = ('item, 'value) RedBlackTrees.PolyMap.t
let map_empty (compare : 'item -> 'item -> int) : ('item, 'value) map = RedBlackTrees.PolyMap.create ~cmp:compare
let map_find : 'item 'value . 'item -> ('item, 'value) map -> 'value = RedBlackTrees.PolyMap.find
(* let map_iter : 'item 'value . ('item -> 'value -> unit) -> ('item, 'value) map -> unit = RedBlackTrees.PolyMap.iter *)
let map_add : 'item 'value . ?debug:(Format.formatter -> 'item * 'value -> unit) -> 'item -> 'value -> ('item, 'value) map -> ('item, 'value) map = RedBlackTrees.PolyMap.add
let map_sorted_keys : 'item 'value . ('item, 'value) map -> 'item list = fun m -> List.map fst @@ RedBlackTrees.PolyMap.bindings m

(** The type [partition] implements a partition of classes of
    equivalent items by means of a map from items to nodes of type
    [node] in trees. *)
type 'item partition = {
    to_string : Format.formatter -> 'item -> unit ;
    compare : 'item -> 'item -> int ;
    map : ('item, 'item node) map ;
}

type 'item t = 'item partition

type 'item repr = 'item

let empty to_string compare = { to_string ; compare ; map = map_empty compare }

let root : ?debug:(Format.formatter -> 'item * 'item node -> unit) -> 'item * height -> 'item t -> 'item t =
  fun ?debug (item, height) { to_string ; compare ; map } ->
  { to_string ; compare ; map = map_add ?debug item (Root height) map }

let link : ?debug:(Format.formatter -> 'item * 'item node -> unit) -> 'item * height -> 'item -> 'item t -> 'item t
  = fun ?debug (src, height) dst { to_string ; compare ; map } ->
  { to_string ; compare ; map = map_add ?debug src (Link (dst, height)) map }

let rec seek (i: 'item) (p: 'item partition) : 'item * height =
  match map_find i p.map with
    Root hi -> i,hi
  | Link (j,_) -> seek j p

let repr i p = fst (seek i p)

(* let is_equiv (i: 'item) (j: 'item) (p: 'item partition) : bool =
 *   try equal p.compare (repr i p) (repr j p) with
 *     Not_found -> false *)

let get_or_set_h ?debug (i: 'item) (p: 'item partition) =
  try seek i p, p with
    Not_found -> let n = i,0 in (n, root ?debug n p)

let get_or_set ?debug (i: 'item) (p: 'item partition) =
  let (i, _h), p = get_or_set_h ?debug i p in (i, p)

(* let mem i p = try Some (repr i p) with Not_found -> None *)

let repr i p = try repr i p with Not_found -> i

type 'item changed_reprs = { demoted_repr : 'item; new_repr : 'item }
type 'item equiv_result = { partition : 'item partition; changed_reprs : 'item changed_reprs }

let equiv ?debug (i: 'item) (j: 'item) (p: 'item partition) : 'item equiv_result =
  let (ri,hi as ni), p = get_or_set_h ?debug i p in
  let (rj,hj as nj), p = get_or_set_h ?debug j p in
  if   equal p.compare ri rj
  then { partition = p; changed_reprs = { demoted_repr = ri; new_repr = rj } }
  else if   hi > hj
       then { partition = link ?debug nj ri p; changed_reprs = { demoted_repr = rj; new_repr = ri } }
       else { partition = link ?debug ni rj (if hi < hj then p else root ?debug (rj, hj+1) p); changed_reprs = { demoted_repr = ri; new_repr = rj } }

(** The call [alias i j p] results in the same partition as [equiv
    i j p], except that [i] is not the representative of its class
    in [alias i j p] (whilst it may be in [equiv i j p]).

    This property is irrespective of the heights of the
    representatives of [i] and [j], that is, of the trees
    implementing their classes. If [i] is not a representative of
    its class before calling [alias], then the height criteria is
    applied (which, without the constraint above, would yield a
    height-balanced new tree). *)
let [@warning "-32" ] alias ~debug (i: 'item) (j: 'item) (p: 'item partition) : 'item partition =
  let (ri,hi as ni), p = get_or_set_h ~debug i p in
  let (rj,hj as nj), p = get_or_set_h ~debug j p in
  if   equal p.compare ri rj
  then p
  else if   hi = hj || equal p.compare ri i
       then link ~debug ni rj @@ root ~debug (rj, max hj (hi+1)) p
       else if hi < hj then link ~debug ni rj p
                       else link ~debug nj ri p

(** {1 iteration over the elements} *)

(* let elements : 'item . 'item partition -> 'item list =
 *   fun { to_string=_; compare=_; map } ->
 *   map_sorted_keys map *)

let partitions : 'item . 'item partition -> 'item list list =
  let compare_lists_by_first cmp la lb =
    match la,lb with
      | [],[] -> 0
      | [],_ -> -1
      | _,[] -> 1
      | a::_, b::_ -> cmp a b in
  fun ({ to_string=_; compare; map } as p) ->
  let aux acc elt =
    RedBlackTrees.PolyMap.update
      (repr elt p)
      (function None -> Some [elt] | Some l -> Some (elt::l))
      acc in
  let grouped = List.fold_left
    aux
    (RedBlackTrees.PolyMap.create ~cmp:compare)
    (map_sorted_keys map) in
  let partitions = RedBlackTrees.PolyMap.bindings grouped in
  (* Sort the elements within partitions and partitions by their smallest element *)
  let partitions = List.map snd partitions in
  let partitions = List.map (List.sort compare) partitions in
  let partitions = List.sort (compare_lists_by_first compare) partitions in
  partitions

(* let get_compare p = p.compare *)

(** {1 Printing} *)

(* let print ppf (p: 'item partition) =
 *   let print i node =
 *     let hi, hj, j =
 *       match node with
 *         Root hi -> hi,hi,i
 *       | Link (j,hi) ->
 *          match map_find j p.map with
 *            Root hj | Link (_,hj) -> hi,hj,j in
 *     let () =
 *       Format.fprintf ppf "%a,%d -> %a,%d\n"
 *         p.to_string i hi p.to_string j hj
 *     in ()
 *   in map_iter print p.map *)

let bindings ({map;_} : 'a t) = RedBlackTrees.PolyMap.bindings map

let pp_node f ppf = function
  Root h -> Format.fprintf ppf "Root (%i)" h
| Link (a,h) -> Format.fprintf ppf "Link (%a,%i)" f a h

let pp f ppf (uf : 'a t) =
  let pp_sep = (fun ppf () -> Format.fprintf ppf " ,@ ") in
  Format.fprintf ppf "@[(%a)@]"
  (Format.pp_print_list ~pp_sep (fun ppf (a,b) -> Format.fprintf ppf "(%a,%a)" f a (pp_node f) b)) (bindings uf)
