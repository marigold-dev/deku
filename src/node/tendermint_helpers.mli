open Tendermint_internals

module type COUNTER = sig
  type 'a t

  val make : 'a t

  val inc : 'a t -> 'a -> int -> 'a t

  val get : 'a t -> 'a -> int

  val count : ('a * int) list -> 'a t

  val cut : 'a t -> threshold:float -> 'a t

  val filter_threshold : ('a * int) list -> threshold:float -> 'a list
end

module Counter : COUNTER

(* FIXME: find another name *)
module IntSet : sig
  include Hashtbl.S with type key = height

  val map_inplace : (height -> 'a -> 'a) -> 'a t -> unit
end
