open Consensus
open Helpers
open Crypto
open Node
open State
open Protocol
open Cmdliner
open Core_deku
open Files

let exits =
  Cmd.Exit.defaults
  @ [Cmd.Exit.info 1 ~doc:"expected failure (might not be a bug)"]

let man = [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

(* TODO: several functions copied from deku-cli. Refactor. *)
type common_options = {
  node_folder : string;
  use_json : bool;
  validator_uris : Uri.t list option;
  style_renderer : Fmt.style_renderer option;
  log_level : Logs.level option;
}

let setup_logs style_renderer log_level use_json =
  (match style_renderer with
  | Some style_renderer -> Fmt_tty.setup_std_outputs ~style_renderer ()
  | None -> Fmt_tty.setup_std_outputs ());
  Logs.set_level log_level;

  (match use_json with
  | true -> Logs.set_reporter (Json_logs_reporter.reporter Fmt.stdout)
  | false -> Logs.set_reporter (Logs_fmt.reporter ()));

  (* disable all non-deku logs *)
  match log_level with
  (* TODO: we probably want a flag like --all_logs or something here instead *)
  | Some Logs.Debug -> ()
  | _ ->
    List.iter
      (fun src ->
        let src_name = Logs.Src.name src in
        if
          (not (String.starts_with ~prefix:"deku" src_name))
          && not (String.equal src_name "application")
        then
          Logs.Src.set_level src (Some Logs.Error))
      (Logs.Src.list ())

let common_options node_folder use_json validator_uris style_renderer log_level
    =
  setup_logs style_renderer log_level use_json;
  { node_folder; use_json; validator_uris; style_renderer; log_level }

let uri =
  let parser uri = Ok (uri |> Uri.of_string) in
  let printer ppf uri = Format.fprintf ppf "%s" (uri |> Uri.to_string) in
  let open Arg in
  conv (parser, printer)

let folder_node =
  let docv = "folder_node" in
  let doc = "Path to the folder containing the node configuration data." in
  let open Arg in
  required & pos 0 (some string) None & info [] ~doc ~docv

let validator_uris =
  let open Arg in
  let docv = "validator_uris" in
  let doc =
    "Comma-separated list of validator URI's to which to broadcast operations"
  in
  let env = Cmd.Env.info "DEKU_VALIDATOR_URIS" in
  value & opt (some (list uri)) None & info ["validator_uris"] ~doc ~docv ~env

let common_options_term =
  let json_logs =
    let docv = "Json logs" in
    let doc = "This determines whether logs will be printed in json format." in
    Arg.(value & flag & info ~doc ~docv ["json-logs"]) in
  let open Term in
  const common_options
  $ folder_node
  $ json_logs
  $ validator_uris
  $ Fmt_cli.style_renderer ~env:(Cmd.Env.info "DEKU_LOG_COLORS") ()
  $ Logs_cli.level
      ~env:
        (Cmd.Env.info
           "DEKU_LOG_VERBOSITY"
           (* TODO: consolidate and document environment options *))
      ()

let read_identity ~node_folder =
  Config_files.Identity.read ~file:(node_folder ^ "/identity.json")

let write_identity ~node_folder =
  Config_files.Identity.write ~file:(node_folder ^ "/identity.json")

let write_interop_context ~node_folder =
  Config_files.Interop_context.write ~file:(node_folder ^ "/tezos.json")

let interop_context node_folder =
  let%await context =
    Config_files.Interop_context.read ~file:(node_folder ^ "/tezos.json") in
  Lwt.return
    (Tezos_interop.make ~rpc_node:context.rpc_node ~secret:context.secret
       ~consensus_contract:context.consensus_contract
       ~discovery_contract:context.discovery_contract
       ~required_confirmations:context.required_confirmations)

let with_validator_uris ?uris node_folder f =
  match uris with
  | Some uris -> f uris
  | None -> (
    let%await interop_context = interop_context node_folder in
    let%await validator_uris =
      Tezos_interop.Consensus.fetch_validators interop_context in
    match validator_uris with
    | Error err -> Lwt.return (`Error (false, err))
    | Ok validator_uris ->
      let uris =
        List.filter_map
          (function
            | _key_hash, Some uri -> Some uri
            | _ -> None)
          validator_uris in
      f uris)

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

let ensure_folder folder =
  let%await exists = Lwt_unix.file_exists folder in
  if exists then
    let%await stat = Lwt_unix.stat folder in
    if stat.st_kind = Lwt_unix.S_DIR then
      await ()
    else
      raise (Invalid_argument (folder ^ " is not a folder"))
  else
    Lwt_unix.mkdir folder 0o700

let edsk_secret_key =
  let parser key =
    match Crypto.Secret.of_string key with
    | Some key -> Ok key
    | _ -> Error (`Msg "Expected EDSK secret key") in
  let printer ppf key = Format.fprintf ppf "%s" (Crypto.Secret.to_string key) in
  let open Arg in
  conv (parser, printer)

let address_tezos_interop =
  let parser string =
    string
    |> Tezos.Address.of_string
    |> Option.to_result ~none:(`Msg "Expected a wallet address.") in
  let printer fmt address =
    Format.fprintf fmt "%s" (Tezos.Address.to_string address) in
  let open Arg in
  conv (parser, printer)

let tezos_required_confirmations =
  let msg = "Expected an integer greater than 0" in
  let parser string =
    match int_of_string_opt string with
    | Some int when int > 0 -> Ok int
    | Some _
    | None ->
      Error (`Msg msg) in
  let printer fmt int = Format.fprintf fmt "%d" int in
  let open Arg in
  conv ~docv:"An integer greater than 0" (parser, printer)

let address_implicit =
  let parser string =
    Option.bind (Address.of_string string) Address.to_key_hash
    |> Option.to_result ~none:(`Msg "Expected a wallet address.") in
  let printer fmt wallet =
    Format.fprintf fmt "%s" (wallet |> Key_hash.to_string) in
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

let start common_options prometheus_port =
  let { node_folder; _ } = common_options in
  node node_folder prometheus_port

let start =
  let minimum_block_delay =
    let docv = "minimum_block_delay" in
    let doc =
      "Determines the minimum time the node will wait before propagating a \
       newly produced block." in
    let open Arg in
    value & opt float 5. & info ["minimum_block_delay"] ~doc ~docv in
  let port =
    let docv = "port" in
    let doc = "The port to listen on for incoming messages." in
    let env = Cmd.Env.info "PORT" ~doc in
    Arg.(value & opt (some int) None & info ~doc ~docv ~env ["port"]) in
  let open Term in
  const start
  $ common_options_term
  $ port
  $ minimum_block_delay
  $ Prometheus_dream.opts

let info_produce_block =
  let doc =
    "Produce and sign a block and broadcast to the network manually, useful \
     when the chain is stale." in
  Cmd.info "produce-block" ~version:"%\226\128\140%VERSION%%" ~doc ~man ~exits

let produce_block { node_folder; validator_uris; _ } =
  let%await identity = read_identity ~node_folder in
  let%await consensus =
    Node_state.get_initial_consensus_state ~folder:node_folder in
  let address = identity.t in
  let block =
    Block.produce ~state:consensus.protocol ~next_state_root_hash:None
      ~author:address ~consensus_operations:[] ~tezos_operations:[]
      ~user_operations:[] in
  Log.info "produced block %a" Protocol.Block.pp block;
  with_validator_uris ?uris:validator_uris node_folder (fun validator_uris ->
      let%await () =
        let open Network in
        broadcast_to_list (module Block_spec) validator_uris { block } in
      Logs.app (fun fmt -> fmt "block.hash: %s" (BLAKE2B.to_string block.hash));
      Lwt.return (`Ok ()))

let produce_block =
  let open Term in
  lwt_ret (const produce_block $ common_options_term)

let info_sign_block =
  let doc =
    "Sign a block hash and broadcast to the network manually, useful when the \
     chain is stale." in
  Cmd.info "sign-block" ~version:"%\226\128\140%VERSION%%" ~doc ~man ~exits

let sign_block node_folder block_hash validator_uris =
  let%await identity = read_identity ~node_folder in
  let signature = Protocol.Signature.sign ~key:identity.secret block_hash in
  with_validator_uris ?uris:validator_uris node_folder (fun validator_uris ->
      let open Network in
      let%await () =
        broadcast_to_list
          (module Signature_spec)
          validator_uris
          { hash = block_hash; signature } in
      Lwt.return (`Ok ()))

let sign_block_term =
  let block_hash =
    let doc = "The block hash to be signed." in
    let open Arg in
    required & pos 1 (some hash) None & info [] ~doc in
  let open Term in
  lwt_ret (const sign_block $ folder_node 0 $ block_hash $ validator_uris)

let info_setup_identity =
  let doc = "Create a validator identity" in
  Cmd.info "setup-identity" ~version:"%\226\128\140%VERSION%%" ~doc

let setup_identity node_folder uri =
  let%await () = ensure_folder node_folder in
  let identity =
    let secret, key = Crypto.Ed25519.generate () in
    let secret, key = (Secret.Ed25519 secret, Key.Ed25519 key) in
    Consensus.make_identity ~secret ~key ~uri in
  let%await () = write_identity ~node_folder identity in
  await (`Ok ())

let setup_identity =
  let self_uri =
    let docv = "self_uri" in
    let doc = "The uri that other nodes should use to connect to this node." in
    let open Arg in
    required & opt (some uri) None & info ["uri"] ~doc ~docv in
  let open Term in
  lwt_ret (const setup_identity $ folder_node 0 $ self_uri)

let info_setup_tezos =
  let doc = "Setup Tezos identity" in
  Cmd.info "setup-tezos" ~version:"%%VERSION%%" ~doc ~man ~exits

let setup_tezos node_folder rpc_node secret consensus_contract
    discovery_contract required_confirmations =
  let%await () = ensure_folder node_folder in
  let%await () =
    write_interop_context ~node_folder
      {
        rpc_node;
        secret;
        consensus_contract;
        discovery_contract;
        required_confirmations;
      } in
  await (`Ok ())

let setup_tezos =
  let tezos_node_uri =
    let docv = "tezos_node_uri" in
    let doc = "The uri of the tezos node." in
    let open Arg in
    required & opt (some uri) None & info ["tezos_rpc_node"] ~doc ~docv in
  let tezos_secret =
    let docv = "tezos_secret" in
    let doc = "The Tezos secret key." in
    let open Arg in
    required
    & opt (some edsk_secret_key) None
    & info ["tezos_secret"] ~doc ~docv in
  let tezos_consensus_contract_address =
    let docv = "tezos_consensus_contract_address" in
    let doc = "The address of the Tezos consensus contract." in
    let open Arg in
    required
    & opt (some address_tezos_interop) None
    & info ["tezos_consensus_contract"] ~doc ~docv in
  let tezos_discovery_contract_address =
    let docv = "tezos_discovery_contract_address" in
    let doc = "The address of the Tezos discovery contract." in
    let open Arg in
    required
    & opt (some address_tezos_interop) None
    & info ["tezos_discovery_contract"] ~doc ~docv in
  let tezos_required_confirmations =
    let docv = "int" in
    let doc =
      "Set the required confirmations. WARNING: Setting below default of 10 \
       can compromise security of the Deku chain." in
    let open Arg in
    value
    & opt tezos_required_confirmations 10
    & info ["unsafe_tezos_required_confirmations"] ~doc ~docv in
  let open Term in
  lwt_ret
    (const setup_tezos
    $ folder_node 0
    $ tezos_node_uri
    $ tezos_secret
    $ tezos_consensus_contract_address
    $ tezos_discovery_contract_address
    $ tezos_required_confirmations)

let info_add_trusted_validator =
  let doc =
    "Helps node operators maintain a list of trusted validators they verified \
     off-chain which can later be used to make sure only trusted validators \
     are added as new validators in the network." in
  Cmd.info "add-trusted-validator" ~version:"%\226\128\140%VERSION%%" ~doc ~man
    ~exits

let add_trusted_validator node_folder address =
  let open Network in
  let%await identity = read_identity ~node_folder in
  let payload =
    let open Trusted_validators_membership_change in
    { address; action = Add } in
  let payload_json_str =
    payload
    |> Trusted_validators_membership_change.payload_to_yojson
    |> Yojson.Safe.to_string in
  let payload_hash = BLAKE2B.hash payload_json_str in
  let signature = Signature.sign ~key:identity.secret payload_hash in
  let%await () =
    Network.request_trusted_validator_membership { signature; payload }
      identity.uri in
  await (`Ok ())

let validator_address =
  let docv = "validator_address" in
  let doc = "The validator address to be added/removed as trusted" in
  let open Arg in
  required & pos 1 (some address_implicit) None & info [] ~docv ~doc

let add_trusted_validator =
  let open Term in
  lwt_ret (const add_trusted_validator $ folder_node 0 $ validator_address)

let info_remove_trusted_validator =
  let doc =
    "Helps node operators maintain a list of trusted validators they verified \
     off-chain which can later be used to make sure only trusted validators \
     are added as new validators in the network." in
  Cmd.info "remove-trusted-validator" ~version:"%\226\128\140%VERSION%%" ~doc
    ~man ~exits

let remove_trusted_validator node_folder address =
  let open Network in
  let%await identity = read_identity ~node_folder in
  let payload =
    let open Trusted_validators_membership_change in
    { address; action = Remove } in
  let payload_json_str =
    payload
    |> Trusted_validators_membership_change.payload_to_yojson
    |> Yojson.Safe.to_string in
  let payload_hash = BLAKE2B.hash payload_json_str in
  let signature = Signature.sign ~key:identity.secret payload_hash in
  let%await () =
    Network.request_trusted_validator_membership { signature; payload }
      identity.uri in
  await (`Ok ())

let info_self =
  let doc = "Shows identity key and address of the node." in
  Cmd.info "self" ~version:"%\226\128\140%VERSION%%" ~doc ~man ~exits

let self node_folder =
  let%await identity = read_identity ~node_folder in
  Format.printf "key: %s\n" (Wallet.to_string identity.key);
  Format.printf "address: %s\n" (Key_hash.to_string identity.t);
  Format.printf "uri: %s\n" (Uri.to_string identity.uri);
  await (`Ok ())

let self =
  let open Term in
  lwt_ret (const self $ folder_node 0)

let remove_trusted_validator =
  let open Term in
  lwt_ret (const remove_trusted_validator $ folder_node 0 $ validator_address)

let info_update_uri =
  let doc = "Updates the uri of the node by the given one." in
  Cmd.info "update-uri" ~version:"%\226\128\140%VERSION%%" ~doc ~man ~exits

let update_uri node_folder uri nonce =
  print_endline (uri |> Uri.to_string);
  let%await interop_context = interop_context node_folder in
  let%await identity = read_identity ~node_folder in
  let%await result =
    Tezos_interop.Discovery.update_validator interop_context identity.secret
      identity.key uri nonce in
  result
  |> Result.fold ~ok:(fun _ -> `Ok ()) ~error:(fun msg -> `Error (false, msg))
  |> await

let update_uri =
  let open Term in
  let new_node_uri =
    let open Arg in
    let docv = "node_uri" in
    let doc =
      "The new uri of the node. The discovery storage will be updated with \
       this uri." in
    required & pos 1 (some uri) None & info [] ~doc ~docv in
  let nonce =
    let open Arg in
    let docv = "nonce" in
    let doc = "The nonce of the transaction." in
    required & pos 2 (some int64) None & info [] ~doc ~docv in
  lwt_ret (const update_uri $ folder_node 0 $ new_node_uri $ nonce)

let default_info =
  let doc = "Deku node" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-node" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

let _ =
  Cmd.eval
  @@ Cmd.group default_info
       [
         Cmd.v (Cmd.info "start") start;
         Cmd.v info_produce_block produce_block;
         Cmd.v info_sign_block sign_block_term;
         Cmd.v info_setup_identity setup_identity;
         Cmd.v info_setup_tezos setup_tezos;
         Cmd.v info_add_trusted_validator add_trusted_validator;
         Cmd.v info_remove_trusted_validator remove_trusted_validator;
         Cmd.v info_self self;
         Cmd.v info_update_uri update_uri;
       ]
