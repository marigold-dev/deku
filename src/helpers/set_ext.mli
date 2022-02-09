include module type of Set
module Make_with_yojson : functor
  (K : sig
     type t
     val compare : t -> t -> int
     val to_yojson : t -> Yojson.Safe.t
     val of_yojson : Yojson.Safe.t -> (t, string) result
   end)
  -> sig
  include module type of Set.Make (K)
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end
