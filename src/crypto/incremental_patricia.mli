module Make (V : sig
  type t

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val hash : t -> BLAKE2b.t
end) : sig
  type value = V.t
  type key = int
  type t

  val find : key -> t -> ((BLAKE2b.t * BLAKE2b.t) list * value) option
  val add : (int -> value) -> t -> t * value
  val empty : t
  val hash : t -> BLAKE2b.t
end
