module Make : functor
  (V : sig
     type t [@@deriving yojson]

     val encoding : t Data_encoding.t
     val hash : t -> BLAKE2b.t
   end)
  -> sig
  type value = V.t
  type key = int
  type t [@@deriving yojson]

  val encoding : t Data_encoding.t
  val empty : t
  val hash : t -> BLAKE2b.t
  val add : (key -> value) -> t -> t * value
  val find : key -> t -> ((BLAKE2b.t * BLAKE2b.t) list * value) option
end
