include module type of List

val fold_left_ok :
  ('a -> 'b -> ('a, 'e) Result.t) -> 'a -> 'b list -> ('a, 'e) Result.t
