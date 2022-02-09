include module type of Map
module Make_with_yojson : functor
  (K : sig
     type t
     val compare : t -> t -> int
     val to_yojson : t -> Yojson.Safe.t
     val of_yojson : Yojson.Safe.t -> (t, string) result
   end)
  -> sig
  include module type of Map.Make (K)
  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    Yojson.Safe.t ->
    ('a t, string) result
end
