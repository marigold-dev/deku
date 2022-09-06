type error_kind =
  | Invalid_parameter
  | Missing_parameter
  | Invalid_body
  | Block_not_found

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

module Repr = struct
  type t = { code : string; msg : string } [@@deriving yojson_of]

  let t_of_error { kind; msg } =
    let code =
      match kind with
      | Invalid_parameter -> "INVALID_PARAMETER"
      | Missing_parameter -> "MISSING_PARAMETER"
      | Invalid_body -> "INVALID_BLOCK"
      | Block_not_found -> "BLOCK_NOT_FOUND"
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
