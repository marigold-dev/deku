open Helpers
open Cmdliner
open Node
open Consensus
open Bin_common
open Protocol
open Crypto

let exits =
  Cmd.Exit.defaults
  @ [Cmd.Exit.info 1 ~doc:"expected failure (might not be a bug)"]

let man = [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

(* TODO: several functions copied from deku-cli. Refactor. *)
let read_identity ~node_folder =
  Files.Identity.read ~file:(node_folder ^ "/identity.json")

let interop_context node_folder =
  let%await context =
    Files.Interop_context.read ~file:(node_folder ^ "/tezos.json") in
  Lwt.return
    (Tezos_interop.make ~rpc_node:context.rpc_node ~secret:context.secret
       ~consensus_contract:context.consensus_contract
       ~discovery_contract:context.discovery_contract
       ~required_confirmations:context.required_confirmations)

let validator_uris ~interop_context =
  Tezos_interop.Consensus.fetch_validators interop_context

let folder_node position =
  let docv = "folder_node" in
  let doc = "The folder where the node lives." in
  let open Arg in
  required & pos position (some string) None & info [] ~doc ~docv

let hash =
  let parser string =
    BLAKE2B.of_string string
    |> Option.to_result ~none:(`Msg "Expected a 256bits BLAKE2b hash.") in
  let printer fmt wallet = Format.fprintf fmt "%s" (BLAKE2B.to_string wallet) in
  let open Arg in
  conv (parser, printer)

let lwt_ret p =
  let open Term in
  ret (const Lwt_main.run $ p)

let update_state state =
  Server.set_state state;
  state

let handle_request (type req res)
    (module E : Network.Request_endpoint
      with type request = req
       and type response = res) handler =
  let handler request =
    let%await body = Dream.body request in
    let%await request = Parallel.decode E.request_of_yojson body in
    Metrics.Networking.inc_network_messages_received E.path;
    Metrics.Networking.measure_network_received_message_size E.path
      (String.length body);
    match request with
    | Ok request -> (
      let response = handler request in
      match response with
      | Ok response ->
        let%await response = Parallel.encode E.response_to_yojson response in
        Dream.json response
      | Error err -> raise (Failure (Flows.string_of_error err)))
    | Error err -> raise (Failure err) in
  Dream.post E.path handler

(* POST /append-block-and-signature *)
(* If the block is not already known and is valid, add it to the pool *)
let handle_received_block =
  handle_request
    (module Network.Block_spec)
    (fun request ->
      Flows.received_block request.block;
      Ok ())

(* POST /append-signature *)
(* Append signature to an already existing block? *)
let handle_received_signature =
  handle_request
    (module Network.Signature_spec)
    (fun request ->
      Flows.received_signature ~hash:request.hash ~signature:request.signature;
      Ok ())

(* POST /block-by-hash *)
(* Retrieve block by provided hash *)
let handle_block_by_hash =
  handle_request
    (module Network.Block_by_hash_spec)
    (fun request ->
      let block = Flows.find_block_by_hash (Server.get_state ()) request.hash in
      Ok block)

(* POST /block-level *)
(* Retrieve height of the chain *)
let handle_block_level =
  handle_request
    (module Network.Block_level)
    (fun () -> Ok { level = Flows.find_block_level (Server.get_state ()) })

(* POST /block-by-level *)
(* Retrieves the block at the given level if it exists, along with
   the timestamp at which it was applied (note the timestamp will be
   slightly different across nodes). *)
let handle_block_by_level =
  handle_request
    (module Network.Block_by_level_spec)
    (fun request ->
      let state = Server.get_state () in
      let block_and_timestamp =
        List.find_opt
          (fun (_, block) ->
            Int64.equal block.Protocol.Block.block_height request.level)
          state.applied_blocks
        |> Option.map (fun (timestamp, block) ->
               Network.Block_by_level_spec.{ block; timestamp }) in
      Ok block_and_timestamp)

(* POST /protocol-snapshot *)
(* Get the snapshot of the protocol (last block and associated signature) *)
let handle_protocol_snapshot =
  handle_request
    (module Network.Protocol_snapshot)
    (fun () ->
      let state = Server.get_state () in
      let snapshots = state.consensus.snapshots in
      let%ok snapshot = Snapshots.get_most_recent_snapshot snapshots in
      Ok
        Network.Protocol_snapshot.
          {
            snapshot;
            additional_blocks = snapshots.additional_blocks;
            last_block = snapshots.last_block;
            last_block_signatures =
              Signatures.to_list snapshots.last_block_signatures;
          })

(* POST /request-nonce *)
(* Unused fow now *)
(* Set a new Number Only Used Once for the selected key *)
(* so that the author can make a proof that he is the owner of the secret and can set the URI *)
let handle_request_nonce =
  handle_request
    (module Network.Request_nonce)
    (fun { uri } ->
      let nonce = Flows.request_nonce (Server.get_state ()) update_state uri in
      Ok { nonce })

(* POST /register-uri *)
(* Set the provided URI of the validator *)
let handle_register_uri =
  handle_request
    (module Network.Register_uri)
    (fun { uri; signature } ->
      Flows.register_uri (Server.get_state ()) update_state ~uri ~signature)

(* POST /user-operation-gossip *)
(* Propagate user operation (core_user.t) over gossip network *)
let handle_receive_user_operation_gossip =
  handle_request
    (module Network.User_operation_gossip)
    (fun request ->
      Flows.received_user_operation request.user_operation;
      Ok ())

(* POST /user-operations-gossip *)
(* Propagate a batch of user operations (core_user.t) over gossip network *)
let handle_receive_user_operations_gossip =
  handle_request
    (module Network.User_operations_gossip)
    (fun request ->
      let operations = request.user_operations in
      List.iter
        (fun operation ->
          (* TODO: quadratic function *)
          Flows.received_user_operation operation)
        operations;
      Ok ())

(* POST /consensus-operation-gossip *)
(* Add operation from consensu to pending operations *)
let handle_receive_consensus_operation =
  handle_request
    (module Network.Consensus_operation_gossip)
    (fun request ->
      Flows.received_consensus_operation (Server.get_state ())
        request.consensus_operation request.signature)

(* POST /trusted-validators-membership *)
(* Add or Remove a new trusted validator *)
let handle_trusted_validators_membership =
  handle_request
    (module Network.Trusted_validators_membership_change)
    (fun request ->
      Flows.trusted_validators_membership ~payload:request.payload
        ~signature:request.signature;
      Ok ())

(* POST /withdraw-proof *)
(* Returns a proof that can be provided to Tezos to fulfill a withdraw *)
let handle_withdraw_proof =
  handle_request
    (module Network.Withdraw_proof)
    (fun { operation_hash } ->
      Ok
        (Flows.request_withdraw_proof (Server.get_state ()) ~hash:operation_hash))

(* POST /ticket-balance *)
(* Returns how much of a ticket a key has *)
let handle_ticket_balance =
  handle_request
    (module Network.Ticket_balance)
    (fun { ticket; address } ->
      let state = Server.get_state () in
      let amount =
        Flows.request_ticket_balance state ~ticket
          ~address:(Core_deku.Address.of_key_hash address) in
      Ok { amount })

let node folder port minimum_block_delay prometheus_port =
  let node =
    Node_state.get_initial_state ~folder ~minimum_block_delay |> Lwt_main.run
  in
  Tezos_interop.Consensus.listen_operations node.Node.State.interop_context
    ~on_operation:(fun operation -> Flows.received_tezos_operation operation);
  Node.Server.start ~initial:node;
  Dream.initialize_log ~level:`Warning ();
  let port =
    match port with
    | Some port -> port
    | None -> Node.Server.get_port () |> Option.value ~default:4440 in
  Log.info "Listening on port %d" port;
  Lwt.all
    [
      Dream.serve ~interface:"0.0.0.0" ~port
      @@ Dream.router
           [
             handle_block_level;
             handle_received_block;
             handle_received_signature;
             handle_block_by_hash;
             handle_block_by_level;
             handle_protocol_snapshot;
             handle_request_nonce;
             handle_register_uri;
             handle_receive_user_operation_gossip;
             handle_receive_user_operations_gossip;
             handle_receive_consensus_operation;
             handle_withdraw_proof;
             handle_ticket_balance;
             handle_trusted_validators_membership;
           ];
      Prometheus_dream.serve prometheus_port;
    ]
  |> Lwt_main.run
  |> ignore

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-node"

let () =
  (* This is needed because Dream will initialize logs lazily *)
  Dream.initialize_log ~enable:false ()

let node json_logs style_renderer level folder prometheus_port =
  (match style_renderer with
  | Some style_renderer -> Fmt_tty.setup_std_outputs ~style_renderer ()
  | None -> Fmt_tty.setup_std_outputs ());
  Logs.set_level level;

  (match json_logs with
  | true -> Logs.set_reporter (Json_logs_reporter.reporter Fmt.stdout)
  | false -> Logs.set_reporter (Logs_fmt.reporter ()));

  (* disable all non-deku logs *)
  List.iter
    (fun src ->
      let src_name = Logs.Src.name src in
      if
        (not (String.starts_with ~prefix:"deku" src_name))
        && not (String.equal src_name "application")
      then
        Logs.Src.set_level src (Some Logs.Error))
    (Logs.Src.list ());
  node folder prometheus_port

let node =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let minimum_block_delay =
    let docv = "minimum_block_delay" in
    let doc =
      "Determines the minimum time the node will wait before propagating a \
       newly produced block." in
    let open Arg in
    value & opt float 5. & info ["minimum_block_delay"] ~doc ~docv in
  let json_logs =
    let docv = "Json logs" in
    let doc = "This determines whether logs will be printed in json format." in
    Arg.(value & flag & info ~doc ~docv ["json-logs"]) in

  let port =
    let docv = "port" in
    let doc = "The port to listen on for incoming messages." in
    let env = Cmd.Env.info "PORT" ~doc in
    Arg.(value & opt (some int) None & info ~doc ~docv ~env ["port"]) in
  let open Term in
  const node
  $ json_logs
  $ Fmt_cli.style_renderer ~env:(Cmd.Env.info "DEKU_LOG_COLORS") ()
  $ Logs_cli.level ()
  $ folder_node
  $ port
  $ minimum_block_delay
  $ Prometheus_dream.opts

let produce_block node_folder =
  let%await identity = read_identity ~node_folder in
  let%await state =
    Node_state.get_initial_state ~minimum_block_delay:0. ~folder:node_folder
  in
  let address = identity.t in
  let block =
    Block.produce ~state:state.consensus.protocol ~next_state_root_hash:None
      ~author:address ~consensus_operations:[] ~tezos_operations:[]
      ~user_operations:[] in
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris ->
    let validator_uris = List.map snd validator_uris |> List.somes in
    let%await () =
      let open Network in
      broadcast_to_list (module Block_spec) validator_uris { block } in
    Format.printf "block.hash: %s\n%!" (BLAKE2B.to_string block.hash);
    Lwt.return (`Ok ())

let produce_block =
  let open Term in
  lwt_ret (const produce_block $ folder_node 0)

let info_produce_block =
  let doc =
    "Produce and sign a block and broadcast to the network manually, useful \
     when the chain is stale." in
  Cmd.info "produce-block" ~version:"%\226\128\140%VERSION%%" ~doc ~man ~exits

let info_sign_block =
  let doc =
    "Sign a block hash and broadcast to the network manually, useful when the \
     chain is stale." in
  Cmd.info "sign-block" ~version:"%\226\128\140%VERSION%%" ~doc ~man ~exits

let sign_block node_folder block_hash =
  let%await identity = read_identity ~node_folder in
  let signature = Protocol.Signature.sign ~key:identity.secret block_hash in
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris ->
    let validator_uris = List.map snd validator_uris |> List.somes in
    let%await () =
      let open Network in
      broadcast_to_list
        (module Signature_spec)
        validator_uris
        { hash = block_hash; signature } in
    Lwt.return (`Ok ())

let sign_block_term =
  let block_hash =
    let doc = "The block hash to be signed." in
    let open Arg in
    required & pos 1 (some hash) None & info [] ~doc in
  let open Term in
  lwt_ret (const sign_block $ folder_node 0 $ block_hash)

let default_info =
  let doc = "Deku node" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-node" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

let _ =
  Cmd.eval
  @@ Cmd.group default_info
       [
         Cmd.v (Cmd.info "start") node;
         Cmd.v info_produce_block produce_block;
         Cmd.v info_sign_block sign_block_term;
       ]
