include module type of List

val fold_left_ok :
  ('a -> 'b -> ('a, 'e) Result.t) -> 'a -> 'b list -> ('a, 'e) Result.t

val chunks_of : length:int -> 'a list -> 'a list list
