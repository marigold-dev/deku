type request_id
type t = request_id [@@deriving eq, ord]

val initial : request_id
val next : request_id -> request_id

module Map : Map.S with type key = request_id
