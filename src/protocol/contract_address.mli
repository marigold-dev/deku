type t [@@deriving eq, ord, yojson]

val of_user_operation_hash : Operation_hash.operation_hash -> t

(*repr*)
val of_b58 : string -> t option
val to_b58 : t -> string
