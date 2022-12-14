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

let make_dump_loop ~sw ~env ~data_folder =
  let resolver_ref = Atomic.make None in
  let domains = Eio.Stdenv.domain_mgr env in

  let rec loop () : unit =
    let promise, resolver = Eio.Promise.create () in
    Atomic.set resolver_ref (Some resolver);
    let current_block, protocol, receipts = Eio.Promise.await promise in
    (try
       Api_state.Storage.write ~env ~data_folder ~current_block ~protocol
         ~receipts
     with exn ->
       Logs.err (fun m -> m "api.storage.failure: %s" (Printexc.to_string exn)));
    loop ()
  in
  let dump ~current_block ~protocol ~receipts =
    match Atomic.exchange resolver_ref None with
    | Some resolver ->
        Eio.Promise.resolve resolver (current_block, protocol, receipts)
    | None -> ()
  in
  ( Eio.Fiber.fork_sub ~sw ~on_error:Deku_constants.async_on_error @@ fun _sw ->
    Eio.Domain_manager.run domains (fun () -> loop ()) );
  dump

let save_block ~sw ~indexer ~block =
  let on_error err =
    Logs.err (fun m -> m "save_block.error: %s\n%!" (Printexc.to_string err))
  in
  (* TODO: better logging *)
  Eio.Fiber.fork_sub ~sw ~on_error @@ fun _sw ->
  Block_storage.save_block ~block indexer

let apply_block ~sw ~state ~block =
  state.current_block <- block;
  save_block ~sw ~indexer:state.indexer ~block;
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
  Logs.info (fun m ->
      m "Applying level %a at time %.3f" Level.pp level (Unix.gettimeofday ()));
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
          | Vm_origination_receipt { operation; _ }
          | Vm_transaction_receipt { operation; _ } ->
              operation
          | Vm_transaction_error { operation; _ } -> operation
        in
        Operation_hash.Map.add hash receipt receipts)
      state.receipts receipts
  in
  state.receipts <- receipts;
  (* TODO: how do we clear the list of receipts ?*)
  state.protocol <- protocol;
  state.in_sync <- true;
  Logs.info (fun m -> m "Writing state for level %a" Level.pp level);
  state.dump ~current_block:block ~protocol ~receipts

let on_accepted_block ~sw ~state ~block =
  let (Block.Block { level = api_level; _ }) = state.current_block in
  let (Block.Block { level; _ }) = block in
  match Level.compare (Level.next api_level) level with
  | 0 -> apply_block ~sw ~state ~block
  | x when x < 0 ->
      Logs.warn (fun m ->
          m "API desynchronized: state at level %a, block at level %a" Level.pp
            api_level Level.pp level);
      state.in_sync <- false
  | _ -> ()

let on_connection ~connection state =
  let (Block.Block { level = api_level; _ }) = state.current_block in
  let (Request.Request { network; _ }) = Request.encode ~above:api_level in
  let (Request.Network.Network_request { raw_header; raw_content }) = network in
  Logs.info (fun m ->
      m "on_connection: sending request for level %a" Level.pp api_level);
  Network_manager.send_request ~connection ~raw_header ~raw_content
    state.network

let on_message ~sw ~raw_header ~raw_content state =
  let header = Message.Header.decode ~raw_header in
  let message = Message.decode ~expected:header ~raw_content in
  let (Message.Message { header = _; content; network = _ }) = message in
  match content with
  | Message.Content.Content_accepted { block; votes = _ } ->
      on_accepted_block ~sw ~state ~block
  | _ -> ()

let listen_to_node ~net ~clock ~on_message ~port ~state =
  let Api_state.{ network; _ } = state in
  let on_connection ~connection = on_connection state ~connection in
  let on_request ~connection:_ ~raw_header:_ ~raw_content:_ = () in
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
       |> Server.with_body (module Encode_operation)
       |> Server.with_body (module Decode_operation)
       |> Server.without_body (module Get_receipt)
       |> Server.with_body (module Compute_contract_hash)
       |> Server.with_body (module Helper_compile_origination)
       |> Server.with_body (module Helper_compile_invocation)
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

let setup_log ?style_renderer ?log_level () =
  (* Without this call, Logs is not thread safe, and causes crashes inside Eio
     when debug logging is enabled. *)
  Logs_threaded.enable ();
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level log_level;
  Logs.set_reporter (Logs_fmt.reporter ());
  (* disable all non-deku logs *)
  List.iter
    (fun src ->
      let src_name = Logs.Src.name src in
      if
        (not (String.starts_with ~prefix:"deku" src_name))
        && not (String.equal src_name "application")
      then Logs.Src.set_level src (Some Logs.Error))
    (Logs.Src.list ())

let main params style_renderer log_level =
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
  setup_log ?style_renderer ?log_level ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Parallel.Pool.run ~env ~domains @@ fun () ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let domains = Eio.Stdenv.domain_mgr env in
  let node_host, node_port =
    match String.split_on_char ':' node_uri with
    | [ node_host; node_port ] -> (node_host, node_port |> int_of_string)
    | _ -> failwith "wrong node uri"
  in
  Logs.info (fun m -> m "API listening on host %s, port %d" node_host node_port);
  let identity =
    let secret =
      Deku_crypto.Secret.Ed25519 (Deku_crypto.Ed25519.Secret.generate ())
    in
    Deku_concepts.Identity.make secret
  in

  let network = Network_manager.make ~identity in
  let indexer =
    let worker = Parallel.Worker.make ~domains ~sw in
    Block_storage.make ~worker ~uri:database_uri
  in

  let dump = make_dump_loop ~sw ~env ~data_folder in

  let state = Api_state.Storage.read ~env ~folder:data_folder in
  let state =
    match state with
    | None ->
        let vm_state = Ocaml_wasm_vm.State.empty in
        let protocol = Protocol.initial_with_vm_state ~vm_state in
        let current_block = Genesis.block in
        let receipts = Operation_hash.Map.empty in
        let (Block { level; _ }) = current_block in
        Logs.info (fun m ->
            m "Starting API with a new state at level %a" Level.pp level);
        Api_state.make ~consensus_address ~indexer ~network ~identity ~protocol
          ~current_block ~receipts ~dump
    | Some state_data ->
        let Api_state.Storage.{ protocol; current_block; receipts } =
          state_data
        in
        let (Block { level; _ }) = current_block in
        Logs.info (fun m -> m "Loading API state at level %a" Level.pp level);
        Api_state.make ~consensus_address ~indexer ~network ~identity ~protocol
          ~current_block ~receipts ~dump
  in

  let on_message = on_message ~sw state in

  Eio.Fiber.all
    [
      (fun () ->
        Network_manager.connect ~net ~clock
          ~nodes:[ (node_host, node_port) ]
          ~on_connection:(on_connection state)
          ~on_request:(fun ~connection:_ ~raw_header:_ ~raw_content:_ -> ())
          ~on_message network);
      (fun () -> start_api ~env ~sw ~port ~state);
      (fun () -> listen_to_node ~net ~clock ~on_message ~port:tcp_port ~state);
    ]

let () =
  let open Cmdliner in
  let info = Cmd.info "deku-api" in
  let term =
    Term.(
      const main $ params_cmdliner_term ()
      $ Fmt_cli.style_renderer ~env:(Cmd.Env.info "DEKU_LOG_COLORS") ()
      $ Logs_cli.level ~env:(Cmd.Env.info "DEKU_API_LOG_VERBOSITY") ())
  in
  let cmd = Cmd.v info term in
  exit (Cmd.eval ~catch:true cmd)
