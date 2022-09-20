open Deku_stdlib
open Deku_crypto
open Deku_gossip
open Piaf

type network = {
  nodes : Uri.t list;
  mutable next_request_id : Request_id.t;
  mutable requests : ((string * string) option -> unit) Request_id.Map.t;
}

type t = network

let error ~message status =
  let response = Response.of_string ~body:message status in
  Lwt.return response

let internal_error error =
  let response = Response.or_internal_error (Error error) in
  Lwt.return response

let raw_expected_hash_header = "X-Raw-Expected-Hash"
let broadcast_endpoint = "/messages"
let request_endpoint = "/request"

let headers ~raw_expected_hash =
  let open Headers in
  (* TODO: maybe add json to Well_known in Piaf*)
  let json = Mime_types.map_extension "json" in
  [
    (Well_known.content_type, json);
    (raw_expected_hash_header, raw_expected_hash);
  ]

let with_raw_content ~request k =
  let body = request.Request.body in
  let%await raw_content = Body.to_string body in
  match raw_content with
  | Ok raw_content -> k raw_content
  | Error error -> internal_error error

let with_raw_expected_hash ~request k =
  let headers = request.Request.headers in
  match Headers.get headers raw_expected_hash_header with
  | Some raw_expected_hash -> k raw_expected_hash
  | None -> error ~message:"?expected_hash is required" `Bad_request

let handler_messages ~on_message request =
  with_raw_expected_hash ~request @@ fun raw_expected_hash ->
  with_raw_content ~request @@ fun raw_content ->
  let () = on_message ~raw_expected_hash ~raw_content in
  let response = Piaf.Response.of_string ~body:"OK" `OK in
  Lwt.return response

let handler_request ~on_request ~network request =
  with_raw_expected_hash ~request @@ fun raw_expected_hash ->
  with_raw_content ~request @@ fun raw_content ->
  let id = network.next_request_id in
  network.next_request_id <- Request_id.next id;

  let response_promise, response_resolver = Lwt.wait () in
  let resolver response =
    network.requests <- Request_id.Map.remove id network.requests;
    Lwt.wakeup_later response_resolver response
  in
  network.requests <- Request_id.Map.add id resolver network.requests;
  on_request ~id ~raw_expected_hash ~raw_content;

  let%await response = response_promise in
  let response =
    match response with
    | Some (raw_expected_hash, raw_content) ->
        let headers = headers ~raw_expected_hash in
        let headers = Headers.of_list headers in
        Piaf.Response.of_string ~headers ~body:raw_content `OK
    | None -> Piaf.Response.of_string ~body:"Not Found" `Not_found
  in
  Lwt.return response

let handler ~on_message ~on_request ~network Server.{ ctx = _; request } =
  let uri = Uri.of_string request.target in
  let meth = request.meth in
  let endpoint = Uri.path uri in
  match (meth, endpoint) with
  | `POST, "/messages" -> handler_messages ~on_message request
  | `POST, "/request" -> handler_request ~on_request ~network request
  | _ ->
      error ~message:"only POST /messages and POST /request are supported"
        `Bad_request

let listen ~port ~on_message ~on_request network =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      (* TODO: piaf error_handler *)
      let%await _server =
        Lwt_io.establish_server_with_client_socket listen_address
          (Server.create ?config:None ?error_handler:None (fun context ->
               (* Format.eprintf "request\n%!"; *)
               handler ~on_message ~on_request ~network context))
      in
      let () = Printf.printf "Listening on port %i\n%!" port in
      Lwt.return_unit)

let connect ~nodes =
  {
    nodes;
    next_request_id = Request_id.initial;
    requests = Request_id.Map.empty;
  }

let post ~raw_expected_hash ~raw_content ~uri =
  let target = Uri.with_path uri broadcast_endpoint in
  (* let target = Uri.to_string target in *)
  (* Format.eprintf "%a <- %s\n%!" Uri.pp_hum _uri target; *)
  let headers = headers ~raw_expected_hash in
  let body = Body.of_string raw_content in
  Client.Oneshot.post ~headers ~body target

let post ~raw_expected_hash ~raw_content ~uri =
  Lwt.async (fun () ->
      let%await post = post ~raw_expected_hash ~raw_content ~uri in
      match post with
      | Ok response -> (
          let%await drain = Body.drain response.body in
          match drain with
          | Ok () -> Lwt.return_unit
          | Error _error ->
              (* Format.eprintf "error.drain: %a\n%!" Error.pp_hum _error; *)
              (* TODO: do something with this error *)
              Lwt.return_unit)
      | Error _error ->
          (* Format.eprintf "error.post: %a\n%!" Error.pp_hum _error; *)
          (* TODO: do something with this error *)
          Lwt.return_unit)

let broadcast ~raw_expected_hash ~raw_content network =
  List.iter (fun uri -> post ~raw_expected_hash ~raw_content ~uri) network.nodes

let post ~raw_expected_hash ~raw_content ~uri =
  let target = Uri.with_path uri request_endpoint in
  let headers = headers ~raw_expected_hash in
  let body = Body.of_string raw_content in
  let%await post = Client.Oneshot.post ~headers ~body target in
  match post with
  | Ok response -> (
      let headers = response.headers in
      match Headers.get headers raw_expected_hash_header with
      | Some raw_expected_hash -> (
          let%await body = Body.to_string response.body in
          match body with
          | Ok body -> Lwt.return (Some (raw_expected_hash, body))
          | Error _error ->
              (* Format.eprintf "error.drain: %a\n%!" Error.pp_hum _error; *)
              (* TODO: do something with this error *)
              Lwt.return_none)
      | None ->
          (* TODO: do something with this error *)
          Lwt.return_none)
  | Error _error ->
      (* Format.eprintf "error.post: %a\n%!" Error.pp_hum _error; *)
      (* TODO: do something with this error *)
      Lwt.return_none

let request_from_uri ~raw_expected_hash ~raw_content ~uri =
  Lwt.try_bind
    (fun () -> post ~raw_expected_hash ~raw_content ~uri)
    (fun response ->
      match response with
      | Some (raw_expected_hash, raw_content) ->
          Lwt.return (Some (raw_expected_hash, raw_content))
      | None -> Lwt.return_none)
    (fun _exn -> (* TODO: do something with this error *) Lwt.return_none)

let rec request ~raw_expected_hash ~raw_content network =
  (* TODO: this is non ideal but works *)
  let size = List.length network.nodes in
  let n = Random.int size in
  let uri = List.nth network.nodes n in

  (* TODO: slow loris *)

  (* TODO: make it cancellable *)
  let%await response = request_from_uri ~raw_expected_hash ~raw_content ~uri in
  match response with
  | Some (raw_expected_hash, raw_content) ->
      Lwt.return (raw_expected_hash, raw_content)
  | None -> request ~raw_expected_hash ~raw_content network

let respond ~id ~raw_expected_hash ~raw_content network =
  match Request_id.Map.find_opt id network.requests with
  | Some resolver -> resolver (Some (raw_expected_hash, raw_content))
  | None ->
      (* TODO: what do I do here? *)
      Format.eprintf "duplicated respond\n%!";
      ()

let not_found ~id network =
  match Request_id.Map.find_opt id network.requests with
  | Some resolver -> resolver None
  | None ->
      (* TODO: what do I do here? *)
      Format.eprintf "duplicated respond\n%!";
      ()
