(*
module RopeImplementation = Rope_implementation
type impl = RopeImplementation.t
type 'a t =
    S : string -> (((impl -> 'a) -> 'b) -> (impl -> 'a) -> 'b) t
  | Other : 'a -> 'a t
val _S : string -> ((impl -> 'a) -> 'b) -> (RopeImplementation.t -> 'a) -> 'b
val z :
  (string -> ((impl -> 'a) -> 'b) -> (RopeImplementation.t -> 'a) -> 'b) t
val _d : ((impl -> int -> 'a) -> 'b) -> (RopeImplementation.t -> 'a) -> 'b
val d : (((impl -> int -> 'a) -> 'b) -> (RopeImplementation.t -> 'a) -> 'b) t
val _s : ((impl -> string -> 'a) -> 'b) -> (RopeImplementation.t -> 'a) -> 'b
val s :
  (((impl -> string -> 'a) -> 'b) -> (RopeImplementation.t -> 'a) -> 'b) t
val start : (RopeImplementation.t -> 'a) -> 'a
val finish : impl -> impl
val ( ~% ) : (((impl -> 'a) -> 'a) -> 'b) t -> 'b
val ( % ) : 'a -> ('a -> 'b) t -> 'b
val ( #% ) : ((impl -> impl) -> 'a -> 'b) -> 'a -> 'b
*)
