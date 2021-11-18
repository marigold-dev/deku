(* Red-black trees according to the following classic paper:

   Chris Okasaki, Red-Black Trees in a Functional
   Setting. J. Funct. Program. 9(4): 471-477 (1999)
*)

type colour = Red | Black

type 'a t = private
  L
| T of colour * 'a t * 'a * 'a t

val empty: 'a t

val is_empty: 'a t -> bool

val count : 'a t -> int

val blackDepth : 'a t -> int

val checkDepth_opt : 'a t -> int option

val is_legal : 'a t -> bool

(* The value of the call [add ~cmp choice x t] is a red-black tree
   augmenting the tree [t] with a node containing the element [x],
   using the comparison function [cmp] (following the same convention
   as [Pervasives.compare]) and, if a value [y] such that [x = y] is
   already present in a node of [t], then the value [choice] denotes
   whether [x] ([New]) or [y] ([Old]) remains in the value of the
   call. Moreover, if [x == y], then [add ~cmp choice x t == t]. *)

type choice = Old | New

val add: ?debug:(Format.formatter -> 'a -> unit) -> cmp:('a -> 'a -> int) -> choice -> 'a -> 'a t -> 'a t

val union: cmp:('a -> 'a -> int) -> choice -> 'a t -> 'a t -> 'a t

(* The value of the call [remove ~cmp x t] is a red-black tree
   containing the same elements as [t] with the exception of the
   element identified by [x]. The type of [x] can be different from
   that of the elements of the tree, for example if the tree is used to
   implement a map, x would be a [key], whereas the elements of the tree
   would be [key, value] pairs. *)

val delete: ?debug:(Format.formatter -> 'b -> unit) -> cmp:('a -> 'b -> int) -> 'a -> 'b t -> 'b t

val delete_opt: ?debug:(Format.formatter -> 'b -> unit) -> cmp:('a -> 'b -> int) -> 'a -> 'b t -> 'b t option

(* The value of the call [find ~cmp x t] is the element [y] belonging
   to a node of the tree [t], such that [cmp x y = true]. If none, the
   exception [Not_found] is raised. *)

val find: cmp:('a -> 'b -> int) -> 'a -> 'b t -> 'b

(* The value of call [find_opt ~cmp x t] is [Some y] if there is an
   element [y] in a node of the tree [t] such that [cmp x y = true],
   and [None] otherwise. *)

val find_opt: cmp:('a -> 'b -> int) -> 'a -> 'b t -> 'b option

(* The value of the call [elements t] is the list of elements in the
   nodes of the tree [t], sorted by increasing order. *)

val elements: 'a t -> 'a list

(* The side-effect of evaluating the call [iter f t] is the successive
   side-effects of the calls [f x], for all elements [x] belonging to
   the nodes of [t], visited in increasing order. *)

val iter: ('a -> unit) -> 'a t -> unit

(* The value of the call [fold_inc f ~init t] is the iteration of the
   function [f] on increasing elements of the nodes of tree [t],
   accumulating the partial results from the initial value of
   [init]. *)

val fold_inc: (elt:'a -> acc:'b -> 'b) -> init:'b -> 'a t -> 'b

(* The value of the call [fold_dec f ~init t] is the iteration of the
   function [f] on decreasing elements of the nodes of tree [t],
   accumulating the partial results from the initial value of
   [init]. *)

val fold_dec: (elt:'a -> acc:'b -> 'b) -> init:'b -> 'a t -> 'b

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit