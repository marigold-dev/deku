module Pool : sig
  type pool
  type t = pool

  val make : domains:int -> pool
  val run : pool -> (unit -> 'a) -> 'a
end

val init_p : Pool.t -> int -> (int -> 'a) -> 'a list
val map_p : Pool.t -> ('a -> 'b) -> 'a list -> 'b list
val filter_map_p : Pool.t -> ('a -> 'b option) -> 'a list -> 'b list
val async : Pool.t -> (unit -> 'a) -> 'a Eio.Promise.t
