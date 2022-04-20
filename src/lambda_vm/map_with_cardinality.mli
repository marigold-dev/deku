module type S = sig
  type key

  type 'a t [@@deriving yojson, eq]

  val empty : 'a t

  (* O(log n) *)
  val add : key -> 'a -> 'a t -> 'a t

  (* O(1) *)
  val cardinal : 'a t -> int

  (* O(log n) *)
  val find : key -> 'a t -> 'a option
end

module Make (K : sig
  type t [@@deriving yojson]
  val compare : t -> t -> int
end) : S with type key = K.t
