open Piaf

type error = { code : string; msg : string; http_code : Piaf.Status.t }
type t = error

module Repr = struct
  type error = { code : string; msg : string } [@@deriving yojson_of]

  let error_of_t t =
    let { code; msg; http_code = _ } = t in
    { code; msg }
end

let yojson_of_t error = Repr.error_of_t error |> Repr.yojson_of_error
let to_string error = Format.sprintf "%s: %s" error.code error.msg

let to_response error =
  let status = error.http_code in
  let body = yojson_of_t error |> Yojson.Safe.to_string in
  Response.of_string ~body status

let method_not_allowed meth path =
  let meth = Piaf.Method.to_string meth in
  {
    code = "METHOD_NOT_ALLOWED";
    msg = Format.sprintf "%s method is not allowed on path %s." meth path;
    http_code = `Method_not_allowed;
  }

let invalid_level value =
  {
    code = "INVALID_LEVEL";
    msg =
      Format.sprintf
        "Cannot parse level: %s. It should be a string representation of int64"
        value;
    http_code = `Bad_request;
  }

let endpoint_not_found endpoint =
  {
    code = "ENDPOINT_NOT_FOUND";
    msg = Format.sprintf "Cannot find endpoint %s" endpoint;
    http_code = `Not_found;
  }

let internal_error error =
  { code = "INTERNAL_ERROR"; msg = error; http_code = `Internal_server_error }
