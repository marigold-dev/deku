(* Red-black trees according to the following classic paper:

   Chris Okasaki, Red-Black Trees in a Functional
   Setting. J. Funct. Program. 9(4): 471-477 (1999)
*)

type colour = Red | Black

type 'a t =
  L
| T of colour * 'a t * 'a * 'a t

let empty = L

let is_empty m = (m = empty)

let blacken = function
  L   -> L
| T (_, left, root, right) -> T (Black, left, root, right)

let redden = function
  L -> failwith "Can't redden a leaf"
| T (_, left, root, right) -> T (Red, left, root, right)

let rec pp f ppf = function
  L   -> Format.fprintf ppf "L (Black)"
| T (c, l, root, r) ->
    Format.fprintf ppf "Int (%s,%a,%a,%a)"
    (match c with  Red -> "Red" | Black -> "Black")
    (pp f) l
    f root
    (pp f) r

let is_legal tree =
  let rec max_black_height = function
    T (c, l, _, r) -> 
      let max = max (max_black_height l) (max_black_height r) in
      if c == Black then max + 1 else max
  | L -> 1
  and min_black_height = function
    T (c, l, _, r) -> 
      let min = min (min_black_height l) (min_black_height r) in
      if c == Black then min + 1 else min
  | L -> 1
  and is_balanced tree = 
    let max = max_black_height tree in
    let min = min_black_height tree in
    Format.printf "Max : %d, min : %d\n%!" max min;
    max = min
  and no_red_red = function
    T (Black, l, _, r) 
  | T (Red, (T (Black,_,_,_)| L as l), _, (T(Black,_,_,_) | L as r))
    -> no_red_red l && no_red_red r
  | L -> true
  | _ -> false
  in 
  if not @@ no_red_red tree then failwith "Red_red";
  if not @@ is_balanced tree then failwith "Unbalanced tree";
  true

let max = function
  | L -> None
  | T (_,_,x,l) ->
  let rec max x = function
    | L -> Some x
    | T (_,_,x,l) -> max x l
  in max x l

let count tree =
  let count = 0 in
  let rec aux count = function
    | L -> count
    | T (_, l,_,r) -> aux (aux (count + 1) l) r
  in aux count tree

let blackDepth tree =
  let rec aux count = function
    | L -> count + 1
    | T (Red        , l, _, _) -> aux count l
    | T (Black      , l, _, _) -> aux (count + 1) l
  in aux 0 tree

exception Not_equal
let rec checkDepth = function 
  | L -> 1
  | T (colour, l, _, r) ->
      let lcount = checkDepth l in
      let rcount = checkDepth r in
      if lcount <> rcount then raise Not_equal
      else match colour with
        Red         -> lcount
      | Black       -> lcount + 1

let checkDepth_opt tree =
  try Some (checkDepth tree) with 
    Not_equal -> None


(*
let rec balance_node = function (* n -> n *)
  | T (Black | DoubleBlack as k, T (Red, T (Red, a, x, b), y, c), z ,d)
  | T (Black | DoubleBlack as k, T (Red, a, x, T (Red, b, y, c)), z, d)
  | T (Black | DoubleBlack as k, a, x, T (Red, T (Red, b, y, c), z, d))
  | T (Black | DoubleBlack as k, a, x, T (Red, b, y, T (Red, c, z, d)))
    -> T (blackDec k, T (Black, a, x, b), y, T (Black, c, z, d))
  
  | T (DoubleBlack, a, x, T (NegBlack, T (Black, b, y, c), z, (T (Black, _, _, _) | L as d)))
    -> T (Black, T (Black, a, x, b), y, balance Black c z (redden d))
  
  | T (DoubleBlack, T (NegBlack, (T (Black, _, _, _) | L as a), x, T (Black, b, y, c)), z, d)
    -> T (Black, balance Black (redden a) x b, y, T (Black, c, z, d))
  | t -> t

and balance c l v r =
  balance_node @@ T (c, l, v, r)
*)


(* Okasaki's balance function *)
let balance colour left(*n*) root right(*n*) =
  match colour, left, root, right with
    Black, T (Red, T (Red, a, x, b), y, c), z, d
  | Black, T (Red, a, x, T (Red, b, y, c)), z, d
  | Black, a, x, T (Red, T (Red, b, y, c), z, d)
  | Black, a, x, T (Red, b, y, T (Red, c, z, d)) ->
      T (Red, T (Black, a, x, b), y, T (Black, c, z, d)) (*n+1*)
  | _ ->
      T (colour, left, root, right) (*n+1 if color black*)


type choice = Old | New

let choose ~old ~new' = function
  Old -> old
| New -> new'

exception Physical_equality

let add ?debug:_ ~cmp choice elt tree =
  let rec insert = function
    L -> T (Red, L, elt, L)  (* A leaf *)
  | T (colour, left, root, right) ->
      let diff = cmp elt root in
      if diff = 0 then
        let root' = choose ~new':elt ~old:root choice
        in if root == root' then raise Physical_equality
           else T (colour, left, root', right)
      else if diff < 0 then 
        balance colour (insert left) root right
      else 
        balance colour left root (insert right)
  in 
  let tree = try blacken (insert tree) with
    Physical_equality -> tree
  in
  tree


let delete : type a b . ?debug:(Format.formatter -> b -> unit) -> cmp:(a -> b -> int) -> a -> b t -> b t = 
fun ?debug:_ ~cmp x tree ->
  let rec del = function
    | L -> raise Not_found 
    | T (k, l, y ,r) as nod -> 
        let c = cmp x y in
        if c < 0 then rebalance_left k (del l) y r
        else if c > 0 then rebalance_right k l y (del r)
        else remove nod

  and remove = function
  (* Remove a leaf *)
  | T (Red  , L, _, L) -> L, false
  | T (Black, L, _, L) -> L, true
  (* Only one child implies the child is red and the parent is black *)
  | T (Black, T (Red, l, v, r), _, L)
  | T (Black, L, _, T (Red, l, v, r)) -> T (Black, l, v, r), false
  (* Two sub-trees*)
  | T (c, l, _, r) ->
    let v = Option.get @@ max l in
    let l' = remove_max l in
      rebalance_left c l' v r
  | L -> failwith "impossible"
  
  and rebalance_left colour (left, is_shorter) value right =
    if is_shorter then 
      match colour, left, value, right with
      (* case 1 : The sibling is red so the parent is black. 
      fix : Perform a rotation. d needs to be turn red which may violate 1.
      but balance restore porperty 1. *)
      | Black, a, x, T (Red, T (Black, b, y, c), z, d)
        -> T (Black, T (Black, a, x, b), y, balance Black c z (redden d)),false
      (* case 2: The sibling is black and has at least a red child. Then he has at least three black children. 
        fix : simple rotation, repaint in black.  *)
      | k, a, x, T (Black, T (Red, b, y, c), z, d)
      | k, a, x, T (Black, b, y, T (Red, c, z, d))
        -> T (k, T (Black, a, x, b), y, T (Black, c, z, d)),false
      (* case 3: The sibling is black with two black child. 
        case 3.i the parent is red, turn the root black and the siblign red
        case 3.ii the parent is black, turn the siblign red and propagate that
        the subtree starting with the parent is shorter by one *)
      | k, x, y, T (Black, c, z, d)
        -> T (Black, x, y, T (Red, c, z, d)),k=Black
      | _ -> failwith "Impossible cases"
    else
      T(colour, left, value, right),false

  and rebalance_right colour left value (right,is_shorter) =
    (* complemaentary as the above *)
    if is_shorter then
      match colour, left, value, right with
      | Black, T (Red, a, x, T (Black, b, y, c)), z, d
        -> T (Black, balance Black (redden a) x b, y, T (Black, c, z, d)),false
      | k, T (Black, T (Red, a, x, b), y, c), z, d
      | k, T (Black, a, x, T (Red, b, y, c)), z, d
        -> T (k, T (Black, a, x, b), y, T (Black, c, z, d)),false
      | k, T (Black, a, x, b), y, z
        -> T (Black, T (Red, a, x, b), y, z),k=Black
      | _ -> failwith "Impossible cases"
    else
      T(colour, left, value, right),false

  and remove_max = 
  function
    T (_,_,_,L) as n -> remove n
  | T (c,l,v,r) -> rebalance_right c l v @@ remove_max r
  | L -> failwith "impossible"

    in fst @@ del tree

let delete_opt ?debug ~cmp elt tree =
  try Some (delete ?debug ~cmp elt tree) with Not_found -> None

let rec find ~cmp elt = function
  L -> raise Not_found
| T (_, left, root, right) ->
    let diff = cmp elt root in
    if diff = 0 then (root)
    else if diff < 0 
      then (find ~cmp elt left)
      else (find ~cmp elt right)

let find_opt ~cmp elt tree =
  try Some (find ~cmp elt tree) with Not_found -> None

(* Inorder iterators *)

let rec iter f = function
  L -> ()
| T (_, left, root, right) -> iter f left; f root; iter f right

let rec inorder acc = function
  L -> acc
| T (_, left, root, right) -> inorder (root :: inorder acc right) left

let elements t = inorder [] t

let union ~cmp choice (tree_a : 'a t) tree_b =
  List.fold_left
    (fun acc elt -> add ~cmp choice elt acc)
    tree_b
    (elements tree_a)
    
let rec fold_inc f ~init = function
  L -> init
| T (_, left, root, right) ->
    fold_inc f ~init:(f ~elt:root ~acc:(fold_inc f ~init left)) right

let rec fold_dec f ~init = function
  L -> init
| T (_, left, root, right) ->
    fold_dec f ~init:(f ~elt:root ~acc:(fold_dec f ~init right)) left
