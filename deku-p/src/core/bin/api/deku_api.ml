open Deku_stdlib
open Handlers
open Deku_indexer
open Api_middlewares
open Deku_network

let start_api ~env ~sw ~indexer ~port ~tezos_consensus_address ~node_port =
  let api_constants =
    Api_state.make ~consensus_address:tezos_consensus_address ~node_port
  in
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
       (* |> Server.with_body (module Post_operation) *)
       |> Server.without_body (module Get_vm_state)
       |> Server.make_handler ~env ~constants:api_constants ~indexer)
  in
  let config = Piaf.Server.Config.create port in
  let server = Piaf.Server.create ~config request_handler in
  let _command = Piaf.Server.Command.start ~sw env server in
  ()

let listen_to_node ~net ~clock =
  let port = 5550 in
  let identity =
    let secret = Deku_crypto.Ed25519.Secret.generate () in
    let secret = Deku_crypto.Secret.Ed25519 secret in
    Deku_concepts.Identity.make secret
  in
  let network = Network_manager.make ~identity in
  let on_connection ~connection:_ = print_endline "connected to the node" in
  let on_request ~connection:_ ~raw_header:_ ~raw_content:_ =
    print_endline "a request appear"
  in
  let on_message ~raw_header:_ ~raw_content:_ = print_endline "on message" in
  let () =
    Network_manager.listen ~net ~clock ~port ~on_connection ~on_request
      ~on_message network
  in
  ()

let start_api ~env ~sw () =
  let port = 8080 in
  let uri = Uri.of_string "sqlite3:/tmp/database.db" in
  let config = Indexer.{ save_blocks = true; save_messages = true } in
  let indexer = Indexer.make ~uri ~config in
  let tezos_consensus_address =
    Deku_tezos.Address.of_string "KT1JFPh1zQfhdhEUxKU9ayetgQd92xXvYg5p"
    |> Option.get
  in
  let node_port = 4440 in
  start_api ~env ~sw ~indexer ~port ~tezos_consensus_address ~node_port

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  Eio.Fiber.all
    [
      (fun () -> start_api ~env ~sw ()); (fun () -> listen_to_node ~net ~clock);
    ]