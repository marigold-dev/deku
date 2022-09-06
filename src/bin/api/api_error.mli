type error_kind = private
  | Invalid_parameter
  | Missing_parameter
  | Invalid_body
  | Block_not_found

type error = private { kind : error_kind; msg : string }
type t = error

val invalid_body : string -> error
val missing_parameter : string -> error
val invalid_parameter : string -> error
val block_not_found : error

(* Utils *)
val yojson_of_t : error -> Yojson.Safe.t
val to_http_code : error -> int
