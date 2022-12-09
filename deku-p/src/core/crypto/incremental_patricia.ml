module Make (V : sig
  type t

  val encoding : t Data_encoding.t
  val hash : t -> BLAKE2b.t
end) =
struct
  type value = V.t
  type key = int

  type tree =
    | Empty
    | Leaf of { value : value; hash : BLAKE2b.t }
    | Node of { left : tree; hash : BLAKE2b.t; right : tree }

  type t = { tree : tree; top_bit : key; last_key : key }

  let is_set bit number = (1 lsl bit) land number <> 0
  let empty_hash = BLAKE2b.hash ""

  let hash_of_t = function
    | Empty -> empty_hash
    | Leaf { hash; _ } | Node { hash; _ } -> hash

  let rec find bit proofs key t =
    match t with
    | Empty -> None
    | Leaf { value; _ } -> Some (List.rev proofs, value)
    | Node { left; right; _ } ->
        let t = match is_set bit key with true -> right | false -> left in
        find (bit - 1) ((hash_of_t left, hash_of_t right) :: proofs) key t

  let find key t =
    if key >= 0 && key <= t.last_key then find (t.top_bit - 1) [] key t.tree
    else None

  let node left right =
    let hash = BLAKE2b.both (hash_of_t left) (hash_of_t right) in
    Node { left; hash; right }

  let rec empty n =
    if n = 0 then Empty
    else
      let tree = empty (n - 1) in
      node tree tree

  let add f t =
    let rec add bit key value t =
      match (bit, t) with
      | -1, Empty -> Leaf { value; hash = V.hash value }
      | _, Node { left; right; _ } -> (
          match is_set bit key with
          | true -> node left (add (bit - 1) key value right)
          | false -> node (add (bit - 1) key value left) right)
      | _ -> assert false
    in
    let key = t.last_key + 1 in
    let value = f key in
    let increase_top_bit = key lsr t.top_bit in
    let top_bit = t.top_bit + increase_top_bit in
    let tree =
      if increase_top_bit = 1 then
        let right = empty (top_bit - 1) in
        node t.tree right
      else t.tree
    in
    let tree = add (top_bit - 1) key value tree in
    ({ tree; top_bit; last_key = key }, value)

  let empty = { top_bit = 0; tree = Empty; last_key = -1 }
  let hash t = hash_of_t t.tree

  (* TODO: ensure this function preserves ordering *)
  let rec elements acc tree =
    match tree with
    | Empty -> acc
    | Leaf { value; hash = _ } -> value :: acc
    | Node { left; hash = _; right } ->
        let acc = elements acc right in
        elements acc left

  let elements t =
    let { tree; _ } = t in
    elements [] tree

  let of_list l =
    List.fold_left
      (fun map value ->
        let map, _value = add (fun _ -> value) map in
        map)
      empty l

  let encoding =
    let open Data_encoding in
    conv elements of_list (list V.encoding)
end
