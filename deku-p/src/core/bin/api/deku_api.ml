open Deku_stdlib
open Handlers
open Deku_block_storage
open Api_middlewares
open Deku_network
open Deku_gossip
open Api_state
open Deku_protocol
open Deku_consensus
open Deku_concepts

let apply_block ~env ~folder ~state ~block =
  state.current_block <- block;
  Block_storage.save_block ~block state.indexer;
  let (Block.Block { level; payload; tezos_operations; _ }) = block in
  let (Payload.Payload payload) = Payload.decode ~payload in
  let payload =
    Parallel.map_p
      (fun string ->
        let operation =
          string |> Data_encoding.Binary.of_string_exn Operation.Signed.encoding
        in
        let (Operation.Signed.Signed_operation { initial; _ }) = operation in
        initial)
      payload
  in
  let protocol, receipts, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations
      state.protocol
  in
  let receipts =
    List.fold_left
      (fun receipts receipt ->
        let open Receipt in
        let hash =
          match receipt with
          | Ticket_transfer_receipt { operation; _ }
          | Withdraw_receipt { operation; _ }
          | Vm_transaction_receipt { operation; _ } ->
              operation
        in
        Operation_hash.Map.add hash receipt receipts)
      state.receipts receipts
  in
  state.receipts <- receipts;
  (* TODO: how do we clear the list of receipts ?*)
  state.protocol <- protocol;
  state.is_sync <- true;
  Api_state.Storage.write ~env ~folder state

let on_accepted_block ~env ~folder ~state ~block =
  let (Block.Block { level = api_level; _ }) = state.current_block in
  let (Block.Block { level; _ }) = block in
  match Level.equal (Level.next api_level) level with
  | true -> apply_block ~env ~folder ~state ~block
  | false ->
      (*This case should not happened thanks to on_connection*)
      ()

let on_connection ~connection state =
  let (Block.Block { level = api_level; _ }) = state.current_block in
  let (Request.Request { network; _ }) = Request.encode ~above:api_level in
  let (Request.Network.Network_request { raw_header; raw_content }) = network in
  Network_manager.send_request ~connection ~raw_header ~raw_content
    state.network

let on_message ~env ~folder ~raw_header ~raw_content state =
  let header = Message.Header.decode ~raw_header in
  let message = Message.decode ~expected:header ~raw_content in
  let (Message.Message { header = _; content; network = _ }) = message in
  match content with
  | Message.Content.Content_accepted { block; votes = _ } ->
      on_accepted_block ~env ~folder ~state ~block
  | _ -> ()

let listen_to_node ~net ~clock ~port ~state =
  let Api_state.{ network; _ } = state in
  let on_connection ~connection = on_connection state ~connection in
  let on_request ~connection:_ ~raw_header:_ ~raw_content:_ = () in
  let on_message ~raw_header:_ ~raw_content:_ = () in
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
       |> Server.without_body (module Get_proof)
       |> Server.without_body (module Get_balance)
       |> Server.without_body (module Get_chain_info)
       |> Server.with_body (module Helpers_operation_message)
       |> Server.with_body (module Helpers_hash_operation)
       |> Server.with_body (module Post_operation)
       |> Server.without_body (module Get_vm_state)
       |> Server.without_body (module Get_vm_state_key)
       |> Server.without_body (module Get_stats)
       |> Server.with_body (module Get_hexa_to_signed)
       |> Server.without_body (module Get_receipt)
       |> Server.with_body (module Compute_contract_hash)
       |> Server.make_handler ~state)
  in
  let config = Piaf.Server.Config.create port in
  let server = Piaf.Server.create ~config request_handler in
  let _ = Piaf.Server.Command.start ~sw env server in
  ()

type params = {
  consensus_address : Deku_tezos.Address.t;
      [@env "DEKU_TEZOS_CONSENSUS_ADDRESS"]
  node_uri : string; [@env "DEKU_API_NODE_URI"]
  port : int; [@env "DEKU_API_PORT"]
  tcp_port : int; [@env "DEKU_API_TCP_PORT"] [@default 5550]
  database_uri : Uri.t; [@env "DEKU_API_DATABASE_URI"]
  domains : int; [@default 8] [@env "DEKU_API_DOMAINS"]
  data_folder : string; [@env "DEKU_API_DATA_FOLDER"]
}
[@@deriving cmdliner]

let main params =
  let {
    consensus_address;
    node_uri;
    port;
    tcp_port;
    database_uri;
    domains;
    data_folder;
  } =
    params
  in
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
  let config = Block_storage.{ save_blocks = true; save_messages = true } in
  let indexer = Block_storage.make ~uri:database_uri ~config in

  let state = Api_state.Storage.read ~env ~folder:data_folder in
  let state =
    match state with
    | None ->
        let vm_state = Ocaml_wasm_vm.State.empty in
        let protocol = Protocol.initial_with_vm_state ~vm_state in
        let current_block = Genesis.block in
        let receipts = Operation_hash.Map.empty in
        Api_state.make ~consensus_address ~indexer ~network ~identity ~protocol
          ~current_block ~receipts
    | Some state_data ->
        let Api_state.Storage.{ protocol; current_block; receipts } =
          state_data
        in
        Api_state.make ~consensus_address ~indexer ~network ~identity ~protocol
          ~current_block ~receipts
  in

  Eio.Fiber.all
    [
      (fun () ->
        Network_manager.connect ~net ~clock
          ~nodes:[ (node_host, node_port) ]
          ~on_connection:(fun ~connection:_ -> ())
          ~on_request:(fun ~connection:_ ~raw_header:_ ~raw_content:_ -> ())
          ~on_message:(on_message ~env ~folder:data_folder state)
          network);
      (fun () -> start_api ~env ~sw ~port ~state);
      (fun () -> listen_to_node ~net ~clock ~port:tcp_port ~state);
    ]

let () =
  let open Cmdliner in
  let info = Cmd.info "deku-api" in
  let term = Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmd.eval ~catch:true cmd)
