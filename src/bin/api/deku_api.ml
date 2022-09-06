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

type params = {
  database_uri : Uri.t; [@env "DEKU_DATABASE_URI"]
  consensus : Address.t; [@env "DEKU_TEZOS_CONSENSUS_ADDRESS"]
  discovery : Address.t; [@env "DEKU_TEZOS_DISCOVERY_ADDRESS"]
}
[@@deriving cmdliner]

let main params =
  let { database_uri; consensus; discovery } = params in
  Lwt_main.run
  @@ let%await () = Api_state.make ~database_uri ~consensus ~discovery in
     Dream.serve
     @@ Dream.router
          [
            make_handler (module Listen_blocks);
            make_handler (module Get_genesis);
            make_handler (module Get_head);
            make_handler (module Get_block_by_level_or_hash);
            make_handler (module Get_level);
            make_handler (module Get_chain_info);
          ]

let () =
  let open Cmdliner in
  let info = Cmd.info "API interface of deku." in
  let term = Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
