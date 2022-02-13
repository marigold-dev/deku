open Helpers
open Tezos

type 'a request_result =
  ( 'a,
    [ `Json_error         of string
    | `Piaf_body          of Piaf.Error.t
    | `Piaf_request       of Piaf.Error.t
    | `Response_of_yojson of string ] )
  result
  Lwt.t
let request uri data =
  let open Piaf in
  let body = Body.of_string data in
  let%await response = Client.Oneshot.post ~body uri in
  match response with
  | Ok response -> (
    let%await body = Piaf.Body.to_string response.body in
    match body with
    | Ok body -> await (Ok body)
    | Error err -> await (Error (`Piaf_body err)))
  | Error err -> await (Error (`Piaf_request err))
let request response_of_yojson uri data =
  let%await body = request uri data in
  match body with
  | Ok body -> (
    try
      let json = Yojson.Safe.from_string body in
      match response_of_yojson json with
      | Ok response -> await (Ok response)
      | Error err -> await (Error (`Response_of_yojson err))
    with
    | Yojson.Json_error err -> await (Error (`Json_error err)))
  | Error err -> await (Error err)

let inject_operations_path = "/injection/operation"
type inject_operations_response = Operation_hash.t [@@deriving yojson]

let inject_operations ~node_uri ~secret ~branch ~operations =
  let signed_forged_operation = Operation.forge ~secret ~branch ~operations in
  (* TODO: should I use ?async *)
  let uri = Uri.with_path node_uri inject_operations_path in
  request inject_operations_response_of_yojson uri signed_forged_operation
