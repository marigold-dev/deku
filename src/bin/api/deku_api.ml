open Deku_stdlib
open Handlers
open Deku_tezos

let error_to_response error =
  let status = Api_error.to_http_code error |> Dream.int_to_status in
  let body = Api_error.yojson_of_t error |> Yojson.Safe.to_string in
  Dream.json ~status body

let make_handler (module Handler : HANDLER) =
  let handler request =
    let%await input = Handler.input_from_request request in
    match input with
    | Error error -> error_to_response error
    | Ok input -> (
        let state = Api_state.get_state () in
        let%await response = Handler.handle input state in
        match response with
        | Ok response ->
            let body =
              Handler.yojson_of_response response |> Yojson.Safe.to_string
            in
            Dream.json ~status:`OK body
        | Error error -> error_to_response error)
  in
  match Handler.meth with
  | `POST -> Dream.post Handler.path handler
  | `GET -> Dream.get Handler.path handler

(* Returns a websocket to the client*)
let get_websocket =
  let handle _request =
    (* Loop that does nothing*)
    let rec listen uuid websocket =
      let%await message = Dream.receive websocket in
      match message with
      | Some _ -> listen uuid websocket
      | None ->
          let%await () = Dream.close_websocket websocket in
          let () = Api_state.remove_websocket uuid in
          Lwt.return_unit
    in
    Dream.websocket ~close:false (fun websocket ->
        let state = Api_state.get_state () in
        let uuid = Api_state.register_websocket websocket state in
        let%await () = listen uuid websocket in
        Lwt.return_unit)
  in
  Dream.get "/websocket" handle

type params = {
  database_uri : Uri.t; [@env "DEKU_DATABASE_URI"]
  consensus : Address.t; [@env "DEKU_TEZOS_CONSENSUS_ADDRESS"]
  discovery : Address.t; [@env "DEKU_TEZOS_DISCOVERY_ADDRESS"]
  port : int; [@env "DEKU_API_PORT"]
  node : Uri.t; [@env "DEKU_API_DEKU_NODE"]
}
[@@deriving cmdliner]

let handler =
  Middlewares.cors
  @@ Dream.router
       [
         get_websocket;
         Dream.scope "/api/v1/" []
           [
             make_handler (module Listen_blocks);
             make_handler (module Get_genesis);
             make_handler (module Get_head);
             make_handler (module Get_block_by_level_or_hash);
             make_handler (module Get_level);
             make_handler (module Get_chain_info);
             make_handler (module Helpers_operation_message);
             make_handler (module Helpers_hash_operation);
             make_handler (module Post_operation);
           ];
       ]

let main params =
  let { database_uri; consensus; discovery; port; node } = params in
  Lwt_main.run
  @@ let%await () = Api_state.make ~database_uri ~consensus ~discovery ~node in
     Dream.serve ~interface:"0.0.0.0" ~port handler

let () =
  let open Cmdliner in
  let info = Cmd.info "API interface of deku." in
  let term = Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
