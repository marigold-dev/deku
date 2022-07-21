open Helpers

module type Post_request_endpoint = sig
  type request [@@deriving yojson]

  type response [@@deriving yojson]

  val path : string
end

module type Get_request_endpoint = sig
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

let raw_get_request path uri =
  let open Piaf in
  let uri = Uri.with_path uri path in
  let%await response = Client.Oneshot.get uri in
  match response with
  | Ok response -> (
    let%await body = Piaf.Body.to_string response.body in
    match body with
    | Ok body -> await body
    | Error _err -> Lwt.fail Error_status)
  | Error _err -> Lwt.fail Error_status

let request request_to_yojson path data uri =
  let%await raw_data = Parallel.encode request_to_yojson data in
  raw_request path raw_data uri

let get_request path uri = raw_get_request path uri

let raw_post path raw_data uri =
  let%await _body = raw_request path raw_data uri in
  await ()

let post (type req) (module E : Post_request_endpoint with type request = req)
    data uri =
  let%await _body = request E.request_to_yojson E.path data uri in
  await ()

let request (type req res)
    (module E : Post_request_endpoint
      with type request = req
       and type response = res) data uri =
  let%await body = request E.request_to_yojson E.path data uri in
  let%await response = Parallel.decode E.response_of_yojson body in
  let response = Result.get_ok response in
  await response

let get_request (type res)
    (module E : Get_request_endpoint with type response = res) uri =
  let%await body = get_request E.path uri in
  let%await response = Parallel.decode E.response_of_yojson body in
  let response = Result.get_ok response in
  await response

let broadcast_to_list (type req res)
    (module E : Post_request_endpoint
      with type request = req
       and type response = res) uris data =
  let%await data = Parallel.encode E.request_to_yojson data in
  uris
  (* TODO: limit concurrency here *)
  |> Lwt_list.iter_p (fun uri ->
         Lwt.catch (fun () -> raw_post E.path data uri) (fun _exn -> await ()))
