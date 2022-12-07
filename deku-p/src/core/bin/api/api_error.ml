open Deku_protocol

type error_kind =
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
      Format.sprintf "The route [%s] does not allow method %s" path
        (Piaf.Method.to_string allowed_meth);
  }

let endpoint_not_found path =
  {
    kind = Endpoint_not_found;
    msg = Format.sprintf "The route [%s] has not been found" path;
  }

let operation_not_found operation_hash =
  {
    kind = Operation_not_found;
    msg =
      Format.sprintf "The operation [%s] was not found, please wait some time."
        (Operation_hash.to_b58 operation_hash);
  }

let operation_is_not_a_withdraw operation_hash =
  {
    kind = Operation_is_not_a_withdraw;
    msg =
      Format.sprintf "The operation [%s] is not a withdraw operation."
        (Operation_hash.to_b58 operation_hash);
  }

let receipt_not_found operation_hash =
  {
    kind = Receipt_not_found;
    msg =
      Format.sprintf
        "The receipt of the operation [%s] is not found, maybe your operation \
         is not yet included"
        (Operation_hash.to_b58 operation_hash);
  }

module Repr = struct
  type t = { code : string; msg : string }

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
      | Endpoint_not_found -> "ENDPOINT_NOT_FOUND"
      | Operation_not_found -> "OPERATION_NOT_FOUND"
      | Operation_is_not_a_withdraw -> "OPERATION_IS_NOT_A_WITHDRAW"
      | Receipt_not_found -> "RECEIPT_NOT_FOUND"
    in
    { code; msg }

  let encoding =
    let open Data_encoding in
    conv
      (fun { code; msg } -> (code, msg))
      (fun (code, msg) -> { code; msg })
      (obj2 (req "code" string) (req "msg" string))
end

let encoding error =
  Repr.t_of_error error |> Data_encoding.Json.construct Repr.encoding

let string_of_error error =
  let Repr.{ code; _ } = Repr.t_of_error error in
  String.capitalize_ascii code

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
  | Endpoint_not_found -> 404
  | Operation_not_found -> 404
  | Operation_is_not_a_withdraw -> 400
  | Receipt_not_found -> 404
