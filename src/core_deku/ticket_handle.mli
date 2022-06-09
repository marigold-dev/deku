type t [@@deriving yojson, ord, eq]
val to_string : t -> string
val of_string : string -> t option

val make : Address.t -> Tezos.Ticket_id.t -> Amount.t -> t
val to_bytes : t -> bytes
val of_bytes : bytes -> t option

module Map : sig
  include Map.S with type key := t
  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    Yojson.Safe.t ->
    ('a t, string) result
end
module Set : sig
  include Set.S with type elt := t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end
