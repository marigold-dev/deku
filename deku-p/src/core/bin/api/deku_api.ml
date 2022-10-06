open Deku_stdlib
open Handlers
open Deku_indexer
open Api_middlewares
open Deku_network
open Deku_gossip

let parse_message_v1 ~raw_header ~raw_content =
  let header = Message.Header.decode ~raw_header in
  let message = Message.decode ~expected:header ~raw_content in
  let (Message.Message { header = _; content; network = _ }) = message in
  match content with
  | Message.Content.Content_accepted { block = _; votes = _ } ->
      print_endline "block accepted"
  | Message.Content.Content_block _ -> print_endline "content block"
  | Message.Content.Content_operation _ -> print_endline "content operation"
  | Message.Content.Content_vote _ -> print_endline "content vote"

let listen_to_node ~net ~clock ~state =
  let port = 5550 in
  let Api_state.{ network; _ } = state in
  let on_connection ~connection:_ = () in
  let on_request ~connection:_ ~raw_header:_ ~raw_content:_ = () in
  let on_accepted_block ~block ~votes:_ =
    let open Api_state in
    state.current_block <- block
  in
  let on_message ~raw_header ~raw_content =
    let header = Message.Header.decode ~raw_header in
    let message = Message.decode ~expected:header ~raw_content in
    let (Message.Message { header = _; content; network = _ }) = message in
    match content with
    | Message.Content.Content_accepted { block; votes } ->
        on_accepted_block ~block ~votes
    | _ -> ()
  in

  let () =
    Network_manager.listen ~net ~clock ~port ~on_connection ~on_request
      ~on_message network
  in
  ()

let start_api ~env ~sw ~port ~state =
  let request_handler =
    cors_middleware @@ no_cache_middleware
    @@ (Server.empty
       |> Server.without_body (module Get_genesis)
       |> Server.without_body (module Get_head)
       |> Server.without_body (module Get_block_by_level_or_hash)
       |> Server.without_body (module Get_level)
       (* |> Server.without_body (module Get_proof) *)
       (* |> Server.without_body (module Get_balance) *)
       |> Server.without_body (module Get_chain_info)
       |> Server.with_body (module Helpers_operation_message)
       |> Server.with_body (module Helpers_hash_operation)
       |> Server.with_body (module Post_operation)
       |> Server.without_body (module Get_vm_state)
       |> Server.make_handler ~state)
  in
  let config = Piaf.Server.Config.create port in
  let server = Piaf.Server.create ~config request_handler in
  let _command = Piaf.Server.Command.start ~sw env server in
  ()

type params = {
  consensus_address : Deku_tezos.Address.t;
      [@env "DEKU_TEZOS_CONSENSUS_ADDRESS"]
  node_uri : string; [@env "DEKU_API_NODE_URI"]
  port : int; [@env "DEKU_API_PORT"]
  database_uri : Uri.t; [@env "DEKU_API_DATABASE_URI"]
  domains : int; [@default 8] [@env "DEKU_API_DOMAINS"]
}
[@@deriving cmdliner]

let main params =
  let { consensus_address; node_uri; port; database_uri; domains } = params in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Parallel.Pool.run ~env ~domains @@ fun () ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  let node_host, node_port =
    match String.split_on_char ':' node_uri with
    | [ node_host; node_port ] -> (node_host, node_port |> int_of_string)
    | _ -> failwith "wrong node uri"
  in

  let identity =
    let secret =
      Deku_crypto.Secret.Ed25519 (Deku_crypto.Ed25519.Secret.generate ())
    in
    Deku_concepts.Identity.make secret
  in

  let network = Network_manager.make ~identity in
  let config = Indexer.{ save_blocks = true; save_messages = true } in
  let indexer = Indexer.make ~uri:database_uri ~config in
  let state = Api_state.make ~consensus_address ~indexer ~network ~identity in
  Eio.Fiber.all
    [
      (fun () ->
        Network_manager.connect ~net ~clock
          ~nodes:[ (node_host, node_port) ]
          ~on_connection:(fun ~connection:_ -> ())
          ~on_request:(fun ~connection:_ ~raw_header:_ ~raw_content:_ -> ())
          ~on_message:(fun ~raw_header:_ ~raw_content:_ -> ())
          network);
      (fun () -> start_api ~env ~sw ~port ~state);
      (fun () -> listen_to_node ~net ~clock ~state);
    ]

let () =
  let open Cmdliner in
  let info = Cmd.info "deku-api" in
  let term = Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmd.eval ~catch:true cmd)
