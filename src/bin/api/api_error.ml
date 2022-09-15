type error_kind =
  | Invalid_parameter
  | Missing_parameter
  | Invalid_body
  | Block_not_found
  | Internal_error
  | Invalid_operation_signature
  | Invalid_operation_source
  | Method_not_allowed

type error = { kind : error_kind; msg : string }
type t = error

let invalid_body msg =
  { kind = Invalid_body; msg = Format.sprintf "Error in body parsing: %s" msg }

let missing_parameter param =
  {
    kind = Missing_parameter;
    msg = Format.sprintf "Missing parameter: %s" param;
  }

let invalid_parameter msg = { kind = Invalid_parameter; msg }

let block_not_found =
  { kind = Block_not_found; msg = "The requested block was not found" }

let internal_error msg = { kind = Internal_error; msg }

let invalid_operation_signature =
  {
    kind = Invalid_operation_signature;
    msg = "The signature of your operation does not match";
  }

let invalid_operation_source =
  {
    kind = Invalid_operation_source;
    msg = "The source of your operation does not match the given key.";
  }

let method_not_allowed path allowed_meth =
  {
    kind = Method_not_allowed;
    msg =
      Format.sprintf "The route [%s] only allows method %s" path
        (Dream.method_to_string allowed_meth);
  }

module Repr = struct
  type t = { code : string; msg : string } [@@deriving yojson_of]

  let t_of_error { kind; msg } =
    let code =
      match kind with
      | Invalid_parameter -> "INVALID_PARAMETER"
      | Missing_parameter -> "MISSING_PARAMETER"
      | Invalid_body -> "INVALID_BODY"
      | Block_not_found -> "BLOCK_NOT_FOUND"
      | Internal_error -> "INTERNAL_ERROR"
      | Invalid_operation_signature -> "INVALID_OPERATION_SIGNATURE"
      | Invalid_operation_source -> "INVALID_OPERATION_SOURCE"
      | Method_not_allowed -> "METHOD_NOT_ALLOWED"
    in
    { code; msg }
end

let yojson_of_t error = Repr.t_of_error error |> Repr.yojson_of_t

let to_http_code { kind; _ } =
  match kind with
  | Invalid_parameter -> 400
  | Missing_parameter -> 400
  | Invalid_body -> 400
  | Block_not_found -> 404
  | Internal_error -> 500
  | Invalid_operation_signature -> 400
  | Invalid_operation_source -> 400
  | Method_not_allowed -> 405
