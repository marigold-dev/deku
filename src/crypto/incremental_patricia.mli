module Make : functor
  (V : sig
     type t [@@deriving yojson]
     val hash : t -> BLAKE2B.t
   end)
  -> sig
  type value = V.t
  type key = int
  type t [@@deriving yojson]
  val empty : t
  val hash : t -> BLAKE2B.t
  val add : (key -> value) -> t -> t * value
  val find : key -> t -> ((BLAKE2B.t * BLAKE2B.t) list * value) option
end
