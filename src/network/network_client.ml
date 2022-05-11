open Helpers
module type Request_endpoint = sig
  type request [@@deriving yojson]
  type response [@@deriving yojson]
  val path : string
end
exception Error_status

let raw_request path raw_data uri =
  let open Piaf in
  let uri = Uri.with_path uri path in
  let body = Body.of_string raw_data in
  let%await response = Client.Oneshot.post ~body uri in
  match response with
  | Ok response -> (
    let%await body = Piaf.Body.to_string response.body in
    match body with
    | Ok body -> await body
    | Error _err -> Lwt.fail Error_status)
  | Error _err -> Lwt.fail Error_status

let request request_to_yojson path data uri =
  let raw_data = request_to_yojson data |> Yojson.Safe.to_string in
  raw_request path raw_data uri

let raw_post path raw_data uri =
  let%await _body = raw_request path raw_data uri in
  await ()

let post (type req) (module E : Request_endpoint with type request = req) data
    uri =
  let%await _body = request E.request_to_yojson E.path data uri in
  await ()

let request (type req res)
    (module E : Request_endpoint with type request = req and type response = res)
    data uri =
  let%await body = request E.request_to_yojson E.path data uri in
  let response =
    Yojson.Safe.from_string body |> E.response_of_yojson |> Result.get_ok in
  await response

let broadcast_to_list (type req res)
    (module E : Request_endpoint with type request = req and type response = res)
    uris data =
  let data = E.request_to_yojson data |> Yojson.Safe.to_string in
  uris
  (* TODO: limit concurrency here *)
  |> Lwt_list.iter_p (fun uri ->
         Lwt.catch (fun () -> raw_post E.path data uri) (fun _exn -> await ()))
