include module type of List
val find_index : ('a -> bool) -> 'a t -> int option
val in_order_uniq : ('a -> 'a -> int) -> 'a t -> 'a t
val fold_left_ok :
  ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result