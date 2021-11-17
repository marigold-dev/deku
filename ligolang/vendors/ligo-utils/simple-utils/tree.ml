[@@@warning "-9"]

module Append = struct
  type 'a t' =
    | Leaf of 'a
    | Node of {
        a : 'a t' ;
        b : 'a t' ;
        size : int ;
        full : bool ;
      }

  type 'a t =
    | Empty
    | Full of 'a t'

  let node (a, b, size, full) = Node {a;b;size;full}

  let rec exists' f = function
    | Leaf s' when f s' -> true
    | Leaf _ -> false
    | Node{a;b} -> exists' f a || exists' f b
  let exists f = function
    | Empty -> false
    | Full x -> exists' f x

  let rec exists_path' f = function
    | Leaf x -> if f x then Some [] else None
    | Node {a;b} -> (
      match exists_path' f a with
      | Some a -> Some (false :: a)
      | None -> (
        match exists_path' f b with
        | Some b -> Some (true :: b)
        | None -> None
      )
    )

  let exists_path f = function
    | Empty -> None
    | Full x -> exists_path' f x

  let exists_path_to index = exists_path (fun (i,_) -> i = index)

  let empty : 'a t = Empty

  let size' = function
    | Leaf _ -> 1
    | Node {size} -> size

  let size = function
    | Empty -> 0
    | Full x -> size' x

  let rec append' x = function
    | Leaf e -> node (Leaf e, Leaf x, 1, true)
    | Node({full=true;size}) as n -> node(n, Leaf x, size + 1, false)
    | Node({a=Node a;b;full=false} as n) -> (
      match append' x b with
      | Node{full=false} as b -> Node{n with b}
      | Node({full=true} as b) -> Node{n with b = Node b ; full = b.size = a.size}
      | Leaf _ -> assert false
    )
    | Node{a=Leaf _;full=false} -> assert false

  let append x = function
    | Empty -> Full (Leaf x)
    | Full t -> Full (append' x t)

  let of_list lst =
    let rec aux = function
      | [] -> Empty
      | hd :: tl -> append hd (aux tl)
    in
    aux @@ List.rev lst

  let rec to_list' t' =
    match t' with
    | Leaf x -> [x]
    | Node {a;b} -> (to_list' a) @ (to_list' b)

  let to_list t =
    match t with
    | Empty -> []
    | Full x -> to_list' x

  let rec fold' leaf node = function
    | Leaf x -> leaf x
    | Node {a;b} -> node (fold' leaf node a) (fold' leaf node b)

  let rec fold_s' : type a b . a -> (a -> b -> a) -> b t' -> a  = fun init leaf -> function
    | Leaf x -> leaf init x
    | Node {a;b} -> fold_s' (fold_s' init leaf a) leaf b

  let fold_ne leaf node = function
    | Empty -> raise (Failure "Tree.Append.fold_ne")
    | Full x -> fold' leaf node x

  let fold_s_ne : type a b . a -> (a -> b -> a) -> b t -> a = fun init leaf -> function
    | Empty -> raise (Failure "Tree.Append.fold_s_ne")
    | Full x -> fold_s' init leaf x

  let fold empty leaf node = function
    | Empty -> empty
    | Full x -> fold' leaf node x

  let rec assoc_opt' : ('a * 'b) t' -> 'a -> 'b option = fun t k ->
    match t with
    | Leaf (k', v) when k = k' -> Some v
    | Leaf _ -> None
    | Node {a;b} -> (
        match assoc_opt' a k with
        | None -> assoc_opt' b k
        | Some v -> Some v
      )

  let assoc_opt : ('a * 'b) t -> 'a -> 'b option = fun t k ->
    match t with
    | Empty -> None
    | Full t' -> assoc_opt' t' k

  let rec pp' : _ -> _ -> 'a t' -> unit = fun f ppf t' ->
    match t' with
    | Leaf x -> Format.fprintf ppf "%a" f x
    | Node {a;b} -> Format.fprintf ppf "N(%a , %a)" (pp' f) a (pp' f) b

  let pp : _ -> _ -> 'a t -> unit = fun f ppf t ->
    match t with
    | Empty -> Format.fprintf ppf "[]"
    | Full x -> Format.fprintf ppf "[%a]" (pp' f) x
end
