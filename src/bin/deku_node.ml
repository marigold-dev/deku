open Cmdliner
open Helpers
open Node
open Bin_common
let ignore_some_errors = function
  | Error #Flows.ignore -> Ok ()
  | v -> v
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
    match request with
    | Ok request -> (
      let response = handler update_state request in
      match response with
      | Ok response ->
        let%await response = Parallel.encode E.response_to_yojson response in
        Dream.json response
      | Error err -> raise (Failure (Flows.string_of_error err)))
    | Error err -> raise (Failure err) in
  Dream.post E.path handler

(* POST /append-block-and-signature *)
(* If the block is not already known and is valid, add it to the pool *)
let handle_received_block_and_signature =
  handle_request
    (module Network.Block_and_signature_spec)
    (fun update_state request ->
      let open Flows in
      let%ok () =
        received_block (Server.get_state ()) update_state request.block
        |> ignore_some_errors in
      let%ok () =
        received_signature (Server.get_state ()) update_state
          ~hash:request.block.hash ~signature:request.signature
        |> ignore_some_errors in
      Ok ())

(* POST /append-signature *)
(* Append signature to an already existing block? *)
let handle_received_signature =
  handle_request
    (module Network.Signature_spec)
    (fun update_state request ->
      let open Flows in
      let%ok () =
        received_signature (Server.get_state ()) update_state ~hash:request.hash
          ~signature:request.signature
        |> ignore_some_errors in
      Ok ())

(* POST /block-by-hash *)
(* Retrieve block by provided hash *)
let handle_block_by_hash =
  handle_request
    (module Network.Block_by_hash_spec)
    (fun _update_state request ->
      let block = Flows.find_block_by_hash (Server.get_state ()) request.hash in
      Ok block)

(* POST /block-level *)
(* Retrieve height of the chain? *)
let handle_block_level =
  handle_request
    (module Network.Block_level)
    (fun _update_state _request ->
      Ok { level = Flows.find_block_level (Server.get_state ()) })

(* POST /protocol-snapshot *)
(* Get the snapshot of the protocol (last block and associated signature) *)
let handle_protocol_snapshot =
  handle_request
    (module Network.Protocol_snapshot)
    (fun _update_state () ->
      let State.{ snapshots; _ } = Server.get_state () in
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
    (fun update_state { uri } ->
      let nonce = Flows.request_nonce (Server.get_state ()) update_state uri in
      Ok { nonce })

(* POST /register-uri *)
(* Set the provided URI of the validator *)
let handle_register_uri =
  handle_request
    (module Network.Register_uri)
    (fun update_state { uri; signature } ->
      Flows.register_uri (Server.get_state ()) update_state ~uri ~signature)

(* POST /user-operation-gossip *)
(* Propagate user operation (core_user.t) over gossip network *)
let handle_receive_user_operation_gossip =
  handle_request
    (module Network.User_operation_gossip)
    (fun update_state request ->
      Flows.received_user_operation (Server.get_state ()) update_state
        request.user_operation)

(* POST /consensus-operation-gossip *)
(* Add operation from consensu to pending operations *)
let handle_receive_consensus_operation =
  handle_request
    (module Network.Consensus_operation_gossip)
    (fun update_state request ->
      Flows.received_consensus_operation (Server.get_state ()) update_state
        request.consensus_operation request.signature)

(* POST /trusted-validators-membership *)
(* Add or Remove a new trusted validator *)
let handle_trusted_validators_membership =
  handle_request
    (module Network.Trusted_validators_membership_change)
    (fun update_state request ->
      Flows.trusted_validators_membership (Server.get_state ()) update_state
        request)

(* POST /withdraw-proof *)
(* Returns a proof that can be provided to Tezos to fulfill a withdraw *)
let handle_withdraw_proof =
  handle_request
    (module Network.Withdraw_proof)
    (fun _ { operation_hash } ->
      Ok
        (Flows.request_withdraw_proof (Server.get_state ()) ~hash:operation_hash))

(* POST /ticket-balance *)
(* Returns how much of a ticket a key has *)
let handle_ticket_balance =
  handle_request
    (module Network.Ticket_balance)
    (fun _update_state { ticket; address } ->
      let state = Server.get_state () in
      let amount = Flows.request_ticket_balance state ~ticket ~address in
      Ok { amount })
let node folder prometheus_port =
  let node = Node_state.get_initial_state ~folder |> Lwt_main.run in
  Tezos_interop.Consensus.listen_operations node.Node.State.interop_context
    ~on_operation:(fun operation ->
      Flows.received_tezos_operation (Server.get_state ()) update_state
        operation);
  Node.Server.start ~initial:node;
  Dream.initialize_log ~level:`Warning ();
  let port = Node.Server.get_port () |> Option.get in
  Lwt.all
    [
      Dream.serve ~interface:"0.0.0.0" ~port
      @@ Dream.router
           [
             handle_block_level;
             handle_received_block_and_signature;
             handle_received_signature;
             handle_block_by_hash;
             handle_protocol_snapshot;
             handle_request_nonce;
             handle_register_uri;
             handle_receive_user_operation_gossip;
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

let setup_log level () =
  (* This is needed because Dream will initialize logs lazily *)
  Dream.initialize_log ~enable:false ();
  (match Sys.getenv_opt "DEKU_LOGS" |> Option.map String.lowercase_ascii with
  | Some "json" -> Logs.set_reporter (Json_logs_reporter.reporter Fmt.stdout)
  | _ -> Logs.set_reporter (Logs.format_reporter ~app:Fmt.stdout ()));
  Logs.set_level level;
  ()

let setup_log = Term.(const setup_log $ Logs_cli.level ())

let node =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  setup_log $ (const node $ folder_node $ Prometheus_dream.opts)

let () = Term.exit @@ Term.(eval (node, Term.info "deku-node"))
