open Helpers
open Error

type 'a method_ =
  | GET : unit method_
  | POST : string method_
let http_request (type a) ~uri ~(method_ : a method_) (data : a) =
  let open Piaf in
  let%await response =
    match method_ with
    | GET -> Client.Oneshot.get uri
    | POST ->
      let body = Body.of_string data in
      Client.Oneshot.post ~body uri in
  match response with
  | Ok response -> (
    let%await body = Piaf.Body.to_string response.body in
    match body with
    | Ok body -> await (Ok body)
    | Error err -> await (Error (Piaf_body err)))
  | Error err -> await (Error (Piaf_request err))

let http_request ~node_uri ~path ~method_ response_of_yojson data =
  let uri = Uri.with_path node_uri path in
  let%await body = http_request ~uri ~method_ data in
  match body with
  | Ok body -> (
    try
      let json = Yojson.Safe.from_string body in
      match response_of_yojson json with
      | Ok response -> await (Ok response)
      | Error err -> await (Error (Response_of_yojson err))
    with
    | Yojson.Json_error err -> await (Error (Json_error err)))
  | Error err -> await (Error err)

let http_get ~node_uri ~path ~of_yojson =
  http_request ~node_uri ~path ~method_:GET of_yojson ()
let http_post ~node_uri ~path ~of_yojson ~data =
  let data = Yojson.Safe.to_string data in
  http_request ~node_uri ~path ~method_:POST of_yojson data
