include module type of Map

module type S_with_yojson = sig
  include Map.S

  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    Yojson.Safe.t ->
    ('a t, string) result
end

module Make_with_yojson : functor
  (K : sig
     type t

     val compare : t -> t -> int

     val to_yojson : t -> Yojson.Safe.t

     val of_yojson : Yojson.Safe.t -> (t, string) result
   end)
  -> sig
  include Map.S with type key = K.t

  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    Yojson.Safe.t ->
    ('a t, string) result
end
