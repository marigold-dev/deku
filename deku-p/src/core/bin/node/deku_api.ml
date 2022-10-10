open Deku_stdlib
open Handlers
include Node

let json_response ~status body =
  let body = Piaf.Body.of_string body in
  let headers =
    Piaf.Headers.of_list
      [ (Piaf.Headers.Well_known.content_type, "application/json") ]
  in
  Piaf.Response.create ~headers ~body status

let error_to_response error =
  let status = Api_error.to_http_code error |> Piaf.Status.of_code in
  let body = Api_error.yojson_of_t error |> Yojson.Safe.to_string in
  json_response ~status body

let method_not_allowed_handler path meth _ =
  Api_error.method_not_allowed path meth |> error_to_response

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

let make_routes ~identity ~env node indexer constants =
  cors_middleware @@ no_cache_middleware
  @@ fun Piaf.Server.Handler.{ request : Piaf.Request.t; _ } ->
  let router =
    Routes.one_of
      [
        Get_genesis.path ~node ~indexer ~constants;
        Get_head.path ~node ~indexer ~constants;
        Get_stats.path ~node ~indexer ~constants;
        Get_level.path ~node ~indexer ~constants;
        Get_chain_info.path ~node ~indexer ~constants;
        Get_block_by_level_or_hash.path ~node ~indexer ~constants;
        Helpers_operation_message.path ~node ~indexer ~constants;
        Helpers_hash_operation.path ~node ~indexer ~constants;
        Post_operation.path ~identity ~env ~node ~indexer ~constants;
        Get_balance.path ~node ~indexer ~constants;
        Get_proof.path ~node ~indexer ~constants;
        Get_vm_state.path ~node ~indexer ~constants;
        Get_vm_state_key.path ~node ~indexer ~constants;
        (* ws_block_monitor; *)
      ]
  in
  match Routes.match' router ~target:request.target with
  | Routes.NoMatch -> Piaf.Response.create `Not_found
  | FullMatch handler | MatchWithTrailingSlash handler -> (
      match handler request with
      | Ok json -> Yojson.Safe.to_string json |> json_response ~status:`OK
      | Error error -> error_to_response error)
