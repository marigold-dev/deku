open Deku_stdlib
open Handlers
open Deku_indexer
include Node

(* Middlewares *)
let cors_middleware handler req =
  let response : Piaf.Response.t = handler req in
  let add_header name value headers = Piaf.Headers.add headers name value in
  let headers =
    response.headers
    |> add_header "Access-Control-Allow-Origin" "*"
    |> add_header "Access-Control-Allow-Headers" "*"
    |> add_header "Allow" "*"
  in
  let response =
    Piaf.Response.create ~version:response.version ~headers ~body:response.body
      response.status
  in
  response

let no_cache_middleware handler req =
  let response : Piaf.Response.t = handler req in
  let headers =
    Piaf.Headers.add response.headers "Cache-Control"
      "max-age=0, no-cache, no-store"
  in
  let response =
    Piaf.Response.create ~version:response.version ~headers ~body:response.body
      response.status
  in
  response

module Server = struct
  type handler =
    env:Eio.Stdenv.t ->
    constants:Api_constants.t ->
    node:node ->
    indexer:Indexer.t ->
    body:string ->
    Piaf.Response.t

  type server = { get : handler Routes.router; post : handler Routes.router }
  type t = server

  let empty =
    let empty = Routes.one_of [] in
    { get = empty; post = empty }

  let json_to_response ~status body =
    let body = body |> Yojson.Safe.to_string |> Piaf.Body.of_string in
    let headers =
      Piaf.Headers.of_list
        [ (Piaf.Headers.Well_known.content_type, "application/json") ]
    in
    Piaf.Response.create ~headers ~body status

  let error_to_response error =
    let status = Api_error.to_http_code error |> Piaf.Status.of_code in
    let body = Api_error.yojson_of_t error in
    json_to_response ~status body

  let add_route route meth server =
    match meth with
    | `GET -> { server with get = Routes.add_route route server.get }
    | `POST -> { server with post = Routes.add_route route server.post }

  let with_body (module Handler : HANDLERS) server =
    let route =
      Routes.map
        (fun path ~env ~constants ~node ~indexer ~body ->
          let body =
            try
              Yojson.Safe.from_string body
              |> Handler.body_of_yojson |> Result.ok
            with exn ->
              Error
                (Api_error.invalid_body
                   (Format.sprintf "cannot parse the body %s"
                      (Printexc.to_string exn)))
          in
          match body with
          | Error err -> error_to_response err
          | Ok body -> (
              let response =
                Handler.handler ~env ~path ~body ~constants ~node ~indexer
              in
              match response with
              | Error error -> error_to_response error
              | Ok response ->
                  let body = Handler.yojson_of_response response in
                  json_to_response ~status:`OK body))
        Handler.route
    in
    add_route route Handler.meth server

  let without_body (module Handler : NO_BODY_HANDLERS) server =
    let route =
      Routes.map
        (fun path ~env ~constants ~node ~indexer ~body:_ ->
          let response = Handler.handler ~env ~path ~constants ~node ~indexer in
          match response with
          | Error error -> error_to_response error
          | Ok response ->
              let body = Handler.yojson_of_response response in
              json_to_response ~status:`OK body)
        Handler.route
    in
    add_route route Handler.meth server

  let make_handler ~env ~constants ~node ~indexer server request =
    let Piaf.Server.Handler.{ request : Piaf.Request.t; _ } = request in
    let target = request.target in
    let meth = request.meth in
    let body = request.body |> Piaf.Body.to_string in
    match body with
    | Error err ->
        Api_error.invalid_body (Piaf.Error.to_string err) |> error_to_response
    | Ok body -> (
        let matched_route =
          match meth with
          | `GET -> Ok (Routes.match' ~target server.get)
          | `POST -> Ok (Routes.match' ~target server.post)
          | _ -> Error (Api_error.method_not_allowed target meth)
        in
        match matched_route with
        | Ok (Routes.FullMatch handler)
        | Ok (Routes.MatchWithTrailingSlash handler) ->
            handler ~env ~constants ~node ~indexer ~body
        | Ok Routes.NoMatch ->
            Api_error.endpoint_not_found target |> error_to_response
        | Error error -> error |> error_to_response)
end

let make_routes ~env node indexer constants =
  let handler =
    Server.empty
    |> Server.without_body (module Get_genesis)
    |> Server.without_body (module Get_head)
    |> Server.without_body (module Get_block_by_level_or_hash)
    |> Server.without_body (module Get_level)
    |> Server.without_body (module Get_proof)
    |> Server.without_body (module Get_balance)
    |> Server.without_body (module Get_chain_info)
    |> Server.with_body (module Helpers_operation_message)
    |> Server.with_body (module Helpers_hash_operation)
    |> Server.with_body (module Post_operation)
    |> Server.without_body (module Get_vm_state)
    |> Server.make_handler ~env ~constants ~node ~indexer
  in
  cors_middleware @@ no_cache_middleware @@ handler
