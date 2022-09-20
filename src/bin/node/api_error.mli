type error_kind = private
  | Invalid_parameter
  | Missing_parameter
  | Invalid_body
  | Block_not_found
  | Internal_error
  | Invalid_operation_signature
  | Invalid_operation_source
  | Method_not_allowed

type error = private { kind : error_kind; msg : string }
type t = error

val invalid_body : string -> error
val missing_parameter : string -> error
val invalid_parameter : string -> error
val block_not_found : error
val internal_error : string -> error
val invalid_operation_signature : error
val invalid_operation_source : error
val method_not_allowed : string -> Dream.method_ -> error

(* Utils *)
val yojson_of_t : error -> Yojson.Safe.t
val to_http_code : error -> int
