open Deku_stdlib
open Handlers
open Deku_indexer
open Api_middlewares

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
       (* |> Server.with_body (module Post_operation) *)
       |> Server.without_body (module Get_vm_state)
       |> Server.make_handler ~env ~state)
  in
  let config = Piaf.Server.Config.create port in
  let server = Piaf.Server.create ~config request_handler in
  let _command = Piaf.Server.Command.start ~sw env server in
  ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Parallel.Pool.run ~env ~domains:8 @@ fun () ->
  let _gossip = Deku_gossip.Gossip.initial in
  let consensus_address =
    Deku_tezos.Address.of_string "KT1JFPh1zQfhdhEUxKU9ayetgQd92xXvYg5p"
    |> Option.get
  in
  let port = 8080 in
  let uri = Uri.of_string "sqlite3:/tmp/database.db" in
  let config = Indexer.{ save_blocks = true; save_messages = true } in
  let indexer = Indexer.make ~uri ~config in
  let node_port = 4440 in
  let state = Api_state.make ~consensus_address ~indexer ~node_port in

  Eio.Fiber.all [ (fun () -> start_api ~env ~sw ~port ~state) ]
