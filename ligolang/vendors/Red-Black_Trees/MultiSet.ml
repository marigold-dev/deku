type 'k t  = { size: int ; m : ('k, int) PolyMap.t }
let create ~cmp = { size = 0; m = PolyMap.create ~cmp }

let get_compare { size=_; m } = PolyMap.get_compare m

let add k { size; m } = {
  size = size + 1;
  m = PolyMap.update k (function None -> Some 1 | Some x -> Some (x + 1)) m
}

let add_list l s = List.fold_left (fun s x -> add x s) s l

let of_list ~cmp l = add_list l (create ~cmp)

let remove k {size;m} = {
  size = size - 1;
  m = PolyMap.update k (function None -> raise Not_found | Some x -> (match x - 1 with 0 -> None | x' -> Some x')) m
}

let union { size = size1; m = m1 } { size = size2; m = m2 } =
  (* TODO : for a slight improvement in performance, iterate over the smallest of the two sets *)
  let small, big = if size1 < size2 then (m1, m2) else (m2, m1) in
  {
    size = size1 + size2;
    m = List.fold_left
        (fun m (k, n) ->
           PolyMap.update k (function None -> Some n | Some existing -> Some (existing + n)) m)
        big
        (PolyMap.bindings small)
  }

let is_empty { size; m=_ } = size == 0

let pp key_pp ppf { size=_; m } = PolyMap.pp key_pp (fun ppf i -> Format.fprintf ppf "%d" i) ppf m

let elements { size=_; m } =
  let rec repeat v = function 0 -> [] | n -> v :: (repeat v (n - 1)) in
  List.flatten (List.map (fun (k,n) -> repeat k n) (PolyMap.bindings m))

let map_elements f set = List.map f (elements set)
