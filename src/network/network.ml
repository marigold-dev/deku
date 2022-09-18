open Deku_stdlib
open Deku_crypto
open Piaf

type network = Network of { clients : Uri.t Key_hash.Map.t }
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
  let response = Piaf.Response.of_string ~body:"OK" `OK in
  Lwt.return response

let handler on_message context =
  with_uri context @@ fun context ->
  with_raw_expected_hash context @@ fun context ->
  with_raw_content context @@ fun context -> dispatch on_message context

let listen ~port ~on_message =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      (* TODO: piaf error_handler *)
      let%await _server =
        Lwt_io.establish_server_with_client_socket listen_address
          (Server.create ?config:None ?error_handler:None (fun context ->
               (* Format.eprintf "request\n%!"; *)
               handler on_message context))
      in
      let () = Printf.printf "Listening on port %i\n%!" port in
      Lwt.return_unit)

let connect ~nodes =
  let clients =
    List.fold_left
      (fun clients (key, uri) -> Key_hash.Map.add key uri clients)
      Key_hash.Map.empty nodes
  in
  Network { clients }

let endpoint = "/messages"

let post ~raw_expected_hash ~raw_content ~uri =
  let headers =
    let open Headers in
    (* TODO: maybe add json to Well_known in Piaf*)
    let json = Mime_types.map_extension "json" in
    [ (Well_known.content_type, json) ]
  in
  let target = Uri.with_path uri endpoint in
  let target =
    Uri.with_query' target [ (expected_hash_query, raw_expected_hash) ]
  in
  (* let target = Uri.to_string target in *)
  (* Format.eprintf "%a <- %s\n%!" Uri.pp_hum _uri target; *)
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

let send ~to_ ~raw_expected_hash ~raw_content server =
  let (Network { clients }) = server in
  match Key_hash.Map.find_opt to_ clients with
  | Some uri -> post ~raw_expected_hash ~raw_content ~uri
  | None -> (* TODO: do something here *) ()

let broadcast ~raw_expected_hash ~raw_content server =
  let (Network { clients }) = server in

  Key_hash.Map.iter
    (fun _key uri -> post ~raw_expected_hash ~raw_content ~uri)
    clients
