open Helpers

module type Request_endpoint = sig
  type request [@@deriving yojson]

  type response [@@deriving yojson]

  val path : string
end

(* FIXME: to be factorized with Request_endpoint *)
module type Pollinate_endpoint = sig
  type request [@@deriving bin_io]

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
  let%await raw_data = Parallel.encode request_to_yojson data in
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
  let%await response = Parallel.decode E.response_of_yojson body in
  let response = Result.get_ok response in
  await response

let broadcast_to_list (type req res)
    (module E : Request_endpoint with type request = req and type response = res)
    uris data =
  let%await data = Parallel.encode E.request_to_yojson data in
  uris
  (* TODO: limit concurrency here *)
  |> Lwt_list.iter_p (fun uri ->
         Lwt.catch (fun () -> raw_post E.path data uri) (fun _exn -> await ()))

let send_over_pollinate (type req res)
    (module E : Pollinate_endpoint with type request = req and type response = res)
    node data =
  let data_bin_io = Pollinate.Util.Encoding.pack E.bin_writer_request data in
  let message : Pollinate.PNode.Message.t =
    {
      category = Pollinate.PNode.Message.Post;
      sub_category_opt = Some ("ChainOperation", E.path);
      id = -1;
      timestamp = Unix.gettimeofday ();
      sender = Pollinate.PNode.Client.address_of !node;
      recipients = [];
      payload = data_bin_io;
      payload_signature = None;
    } in
  let _ = Pollinate.PNode.Client.post node message in
  Lwt.return_unit
