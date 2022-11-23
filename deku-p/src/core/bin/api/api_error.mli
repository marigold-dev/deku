open Deku_protocol

type error_kind = private
  | Invalid_parameter
  | Missing_parameter
  | Invalid_body
  | Block_not_found
  | Internal_error
  | Invalid_operation_signature
  | Invalid_operation_source
  | Method_not_allowed
  | Endpoint_not_found
  | Operation_not_found
  | Operation_is_not_a_withdraw
  | Receipt_not_found

type error = private { kind : error_kind; msg : string }
type t = error

val invalid_body : string -> error
val missing_parameter : string -> error
val invalid_parameter : string -> error
val block_not_found : error
val internal_error : string -> error
val invalid_operation_signature : error
val invalid_operation_source : error
val method_not_allowed : string -> Piaf.Method.t -> error
val endpoint_not_found : string -> error
val operation_not_found : Operation_hash.t -> error
val operation_is_not_a_withdraw : Operation_hash.t -> error
val receipt_not_found : Operation_hash.t -> error

(* Utils *)
val string_of_error : error -> string
val yojson_of_t : error -> Yojson.Safe.t
val to_http_code : error -> int
