module Pool : sig
  type pool
  type t = pool

  val make : domains:int -> pool
  val run : pool -> (unit -> 'a) -> 'a
end

val init_p : int -> (int -> 'a) -> 'a list
val map_p : ('a -> 'b) -> 'a list -> 'b list
val filter_map_p : ('a -> 'b option) -> 'a list -> 'b list
val async : (unit -> 'a) -> 'a Eio.Promise.t
