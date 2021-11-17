(* Destructive implementation of union/find with height-balanced
   forests but without path compression: O(n*log(n)). *)

module Make (Item: Partition.Item) =
  struct

    type item = Item.t

    let equal i j = Item.compare i j = 0

    type height = int

    (** Each equivalence class is implemented by a Catalan tree linked
        upwardly and otherwise is a link to another node. Those trees
        are height-balanced. The type [node] implements nodes in those
        trees. *)
    type node = {
                item : item;
      mutable height : int;
      mutable parent : node
    }

    module ItemMap = Map.Make (Item)

    (** The type [partition] implements a partition of classes of
        equivalent items by means of a map from items to nodes of type
        [node] in trees. *)
    type partition = node ItemMap.t

    type t = partition

    let empty = ItemMap.empty

    (** The impure function [repr] is faster than a pure
        implementation in the worst case because, in the latter case,
        the cost is O(log n) for accessing each node in the path to
        the root, whereas, in the former, only the access to the first
        node in the path incurs a cost of O(log n) -- the other nodes
        are accessed in constant time by following the [next] field of
        type [node]. *)
    let seek (i: item) (p: partition) : node =
      let rec find_root node =
        if node.parent == node then node else find_root node.parent
      in find_root (ItemMap.find i p)

    let repr i p = (seek i p).item

    let is_equiv (i: item) (j: item) (p: partition) : bool =
      try equal (repr i p) (repr j p) with
        Not_found -> false

    let get_or_set item (p: partition) =
      try seek item p, p with
        Not_found ->
          let rec loop = {item; height=0; parent=loop}
          in loop, ItemMap.add item loop p

    let mem i p = try Some (repr i p) with Not_found -> None

    let repr i p = try repr i p with Not_found -> i

    let link src dst = src.parent <- dst

    let equiv (i: item) (j: item) (p: partition) : partition =
      let ni,p  = get_or_set i p in
      let nj,p  = get_or_set j p in
      let hi,hj = ni.height, nj.height in
      let () =
        if   not (equal ni.item nj.item)
        then if   hi > hj
             then link nj ni
             else (link ni nj; nj.height <- max hj (hi+1))
      in p

    let alias (i: item) (j: item) (p: partition) : partition =
      let ni,p  = get_or_set i p in
      let nj,p  = get_or_set j p in
      let hi,hj = ni.height, nj.height in
      let () =
        if   not (equal ni.item nj.item)
        then if   hi = hj || equal ni.item i
             then (link ni nj; nj.height <- max hj (hi+1))
             else if hi < hj then link ni nj
                             else link nj ni
      in p

    (* Printing *)

    let print (p: partition) =
      let buffer = Buffer.create 80 in
      let print _ node =
        let link =
          Printf.sprintf "%s,%d -> %s,%d\n"
            (Item.to_string node.item) node.height
            (Item.to_string node.parent.item) node.parent.height
        in Buffer.add_string buffer link
      in ItemMap.iter print p; buffer

  end
