open Helpers

module type Request_endpoint = sig
  type request [@@deriving yojson]

  type response [@@deriving yojson]

  val path : string
end

module Pollinate_utils = struct
  open Bin_prot.Std

  type category = ChainOperation [@@deriving bin_io]

  type action =
    | Append_block
    | Append_signature
  [@@deriving bin_io]

  type payload = {
    category : category;
    action : action;
    data : bytes;
    signature_payload : bytes option;
  }
  [@@deriving bin_io]

  let uri_to_pollinate : Uri.t -> Pollinate.Address.t =
   fun uri ->
    Log.debug "Translating Uri.t to Pollinate.Address.t";
    let address =
      match Uri.host uri with
      | Some "localhost" -> "127.0.0.1"
      | Some address -> address
      | _ -> failwith "Could not retrieve address from uri" in
    let port =
      match Uri.port uri with
      | Some port -> port + 100 (* ugly fix to avoif using the HTTP port *)
      | None -> failwith "Could not retrieve port from uri" in
    Pollinate.Address.create address port
end

(* FIXME: to be factorized with Request_endpoint *)
module type Pollinate_endpoint = sig
  type request [@@deriving bin_io]

  val category : Pollinate_utils.category

  val action : Pollinate_utils.action
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

let send_over_pollinate (type req)
    (module E : Pollinate_endpoint with type request = req) node data recipients
    =
  let data_bin_io = Pollinate.Util.Encoding.pack E.bin_writer_request data in
  let payload =
    Pollinate.Util.Encoding.pack Pollinate_utils.bin_writer_payload
      Pollinate_utils.
        {
          category = E.category;
          action = E.action;
          data = data_bin_io;
          signature_payload = None;
        } in
  let message : Pollinate.PNode.Message.t =
    {
      pollinate_category = Pollinate.PNode.Message.Post;
      request_ack = false;
      id = -1;
      timestamp = Unix.gettimeofday ();
      sender = Pollinate.PNode.Client.address_of !node;
      recipients = [];
      payload;
    } in
  let%await () = Pollinate.PNode.Client.broadcast node message recipients in
  Lwt.return_unit
