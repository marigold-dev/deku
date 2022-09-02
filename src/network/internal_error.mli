type error = private { code : string; msg : string; http_code : Piaf.Status.t }
type t = error

val to_string : t -> string
val to_response : t -> Piaf.Response.t
val method_not_allowed : Piaf.Method.t -> string -> error
val invalid_level : string -> error
val endpoint_not_found : string -> error
val internal_error : string -> error
val block_not_found : error
val invalid_block_hash : string -> error
