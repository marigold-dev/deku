module type S = sig
  type key

  type 'a t

  val empty : 'a t

  (* O(log n) *)
  val add : key -> 'a -> 'a t -> 'a t

  (* O(1) *)
  val cardinal : 'a t -> int

  (* O(log n) *)
  val find : key -> 'a t -> 'a option

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module Make (K : Map.OrderedType) : S with type key = K.t
