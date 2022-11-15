module Pool : sig
  val run : env:Eio.Stdenv.t -> domains:int -> (unit -> 'a) -> 'a
end

module Worker : sig
  type worker
  type t = worker

  val make : domains:#Eio.Domain_manager.t -> sw:Eio.Switch.t -> worker
  val schedule : worker -> (unit -> 'a) -> 'a
end

val init_p : int -> (int -> 'a) -> 'a list
val map_p : ('a -> 'b) -> 'a list -> 'b list
val filter_map_p : ('a -> 'b option) -> 'a list -> 'b list
val parallel : (unit -> 'a) -> 'a
