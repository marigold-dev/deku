module Pool : sig
  val run : env:Eio.Stdenv.t -> domains:int -> (unit -> 'a) -> 'a
end

val init_p : int -> (int -> 'a) -> 'a list
val map_p : ('a -> 'b) -> 'a list -> 'b list
val filter_map_p : ('a -> 'b option) -> 'a list -> 'b list
val parallel : (unit -> 'a) -> 'a
