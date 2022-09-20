open Deku_stdlib
open Piaf_lwt

type network = Network of { clients : (Uri.t * Client.t option ref) list }
type t = network

let error ~message status =
  let response = Response.of_string ~body:message status in
  Lwt.return response

let internal_error error =
  let response = Response.or_internal_error (Error error) in
  Lwt.return response

let expected_hash_query = "hash"

let with_uri Server.{ ctx = _; request } next =
  let uri = Uri.of_string request.target in
  let meth = request.meth in
  let endpoint = Uri.path uri in
  match (meth, endpoint) with
  | `POST, "/messages" -> next Server.{ ctx = uri; request }
  | _ -> error ~message:"only POST /messages is supported" `Bad_request

let with_raw_expected_hash Server.{ ctx = uri; request } next =
  match Uri.get_query_param uri expected_hash_query with
  | Some raw_expected_hash -> next Server.{ ctx = raw_expected_hash; request }
  | None -> error ~message:"?expected_hash is required" `Bad_request

let with_raw_content Server.{ ctx = raw_expected_hash; request } next =
  let body = request.body in
  let%await raw_content = Body.to_string body in
  match raw_content with
  | Ok raw_content ->
      next Server.{ ctx = (raw_expected_hash, raw_content); request }
  | Error error -> internal_error error

let dispatch on_message
    Server.{ ctx = raw_expected_hash, raw_content; request = _ } =
  let () = on_message ~raw_expected_hash ~raw_content in
  let response = Piaf_lwt.Response.of_string ~body:"OK" `OK in
  Lwt.return response

let handler on_message context =
  with_uri context @@ fun context ->
  with_raw_expected_hash context @@ fun context ->
  with_raw_content context @@ fun context -> dispatch on_message context

let listen ~port ~on_message =
  let listen_address = Unix.(ADDR_INET (inet_addr_any, port)) in
  Lwt.async (fun () ->
      (* TODO: piaf error_handler *)
      let%await _server =
        Lwt_io.establish_server_with_client_socket listen_address
          (Server.create ?config:None ?error_handler:None (fun context ->
               (* Format.eprintf "request\n%!"; *)
               handler on_message context))
      in
      Logs.info (fun m -> m "Listening on port %i" port);
      Lwt.return_unit)

(* TODO: put this somewhere else *)
let reconnection_timeout = 5.0

let rec connection_loop ref ~uri =
  let config = Config.{ default with flush_headers_immediately = true } in
  let%await client = Client.create ~config uri in
  match client with
  | Ok client ->
      Logs.debug (fun m ->
          m "Network: connected successfully to peer %a" Uri.pp uri);
      ref := Some client;
      Lwt.return_unit
  | Error error ->
      Logs.warn (fun m ->
          m "Network: connection error %a" Piaf_lwt.Error.pp_hum error);
      (* TODO: do something with this error*)
      let%await () = Lwt_unix.sleep reconnection_timeout in
      connection_loop ref ~uri

let connect ~nodes =
  let clients =
    List.map
      (fun uri ->
        let ref = ref None in
        let () = Lwt.async (fun () -> connection_loop ref ~uri) in
        (uri, ref))
      nodes
  in
  Network { clients }

let endpoint = "/messages"

let post ~raw_expected_hash ~raw_content ~uri:_uri client =
  let headers =
    let open Headers in
    (* TODO: maybe add json to Well_known in Piaf*)
    let json = Mime_types.map_extension "json" in
    [ (Well_known.content_type, json) ]
  in
  let target = Uri.of_string endpoint in
  let target =
    Uri.with_query' target [ (expected_hash_query, raw_expected_hash) ]
  in
  let target = Uri.to_string target in
  (* Format.eprintf "%a <- %s\n%!" Uri.pp_hum _uri target; *)
  let body = Body.of_string raw_content in
  Client.post client ~headers ~body target

let post ~raw_expected_hash ~raw_content ~uri client =
  Lwt.async (fun () ->
      Logs.debug (fun m ->
          m "Network: Posting message to %a: %s" Uri.pp uri raw_content);
      let%await post = post ~raw_expected_hash ~raw_content ~uri client in
      match post with
      | Ok response -> (
          let%await drain = Body.drain response.body in
          match drain with
          | Ok () ->
              Logs.debug (fun m -> m "Post successful");
              Lwt.return_unit
          | Error _error ->
              Format.eprintf "error.drain: %a\n%!" Error.pp_hum _error;
              (* TODO: do something with this error *)
              Lwt.return_unit)
      | Error _error ->
          Format.eprintf "error.post: %a" Error.pp_hum _error;
          (* TODO: do something with this error *)
          Lwt.return_unit)

let broadcast ~raw_expected_hash ~raw_content server =
  let (Network { clients }) = server in

  List.iter
    (fun (uri, client) ->
      match !client with
      | Some client -> post ~raw_expected_hash ~raw_content ~uri client
      | None -> ())
    clients
