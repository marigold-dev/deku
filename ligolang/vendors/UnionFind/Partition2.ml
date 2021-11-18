(** Persistent implementation of the Union/Find algorithm with
    height-balanced forests and no path compression. *)

module Make (Item: Partition.Item) =
  struct

    type item = Item.t

    let equal i j = Item.compare i j = 0

    type height = int

    (** Each equivalence class is implemented by a Catalan tree linked
        upwardly and otherwise is a link to another node. Those trees
        are height-balanced. The type [node] implements nodes in those
        trees. *)
    type node =
      Root of height
      (** The value of [Root h] denotes the root of a tree, that is,
          the representative of the associated class.  The height [h]
          is that of the tree, so a tree reduced to its root alone has
          heigh 0. *)

    | Link of item * height
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

    module ItemMap = Map.Make (Item)

    (** The type [partition] implements a partition of classes of
        equivalent items by means of a map from items to nodes of type
        [node] in trees. *)
    type partition = node ItemMap.t

    type t = partition

    let empty = ItemMap.empty

    let root (item, height) = ItemMap.add item (Root height)

    let link (src, height) dst = ItemMap.add src (Link (dst, height))

    let rec seek (i: item) (p: partition) : item * height =
      match ItemMap.find i p with
           Root hi -> i,hi
      | Link (j,_) -> seek j p

    let repr i p = fst (seek i p)

    let is_equiv (i: item) (j: item) (p: partition) : bool =
      try equal (repr i p) (repr j p) with
        Not_found -> false

    let get_or_set (i: item) (p: partition) =
      try seek i p, p with
        Not_found -> let n = i,0 in (n, root n p)

    let mem i p = try Some (repr i p) with Not_found -> None

    let repr i p = try repr i p with Not_found -> i

    let equiv (i: item) (j: item) (p: partition) : partition =
      let (ri,hi as ni), p = get_or_set i p in
      let (rj,hj as nj), p = get_or_set j p in
      if   equal ri rj
      then p
      else if   hi > hj
           then link nj ri p
           else link ni rj (if hi < hj then p else root (rj, hj+1) p)

    (** The call [alias i j p] results in the same partition as [equiv
        i j p], except that [i] is not the representative of its class
        in [alias i j p] (whilst it may be in [equiv i j p]).

        This property is irrespective of the heights of the
        representatives of [i] and [j], that is, of the trees
        implementing their classes. If [i] is not a representative of
        its class before calling [alias], then the height criteria is
        applied (which, without the constraint above, would yield a
        height-balanced new tree). *)
    let alias (i: item) (j: item) (p: partition) : partition =
      let (ri,hi as ni), p = get_or_set i p in
      let (rj,hj as nj), p = get_or_set j p in
      if   equal ri rj
      then p
      else if   hi = hj || equal ri i
           then link ni rj @@ root (rj, max hj (hi+1)) p
           else if hi < hj then link ni rj p
                           else link nj ri p

    (** {1 Printing} *)

    let print (p: partition) =
      let buffer = Buffer.create 80 in
      let print i node =
        let hi, hj, j =
          match node with
            Root hi -> hi,hi,i
          | Link (j,hi) ->
              match ItemMap.find j p with
                Root hj | Link (_,hj) -> hi,hj,j in
        let link =
          Printf.sprintf "%s,%d -> %s,%d\n"
                         (Item.to_string i) hi (Item.to_string j) hj
        in Buffer.add_string buffer link
      in ItemMap.iter print p; buffer

  end
