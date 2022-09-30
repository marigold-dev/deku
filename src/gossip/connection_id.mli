type connection_id
type t = connection_id [@@deriving show, eq, ord]

val initial : connection_id
val next : connection_id -> connection_id

module Map : Map.S with type key = connection_id
