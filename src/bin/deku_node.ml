open Helpers
open Cmdliner
open Node
open Consensus
open Bin_common

let update_state state = Server.set_state state ; state

let handle_request (type req res)
    (module E : Network.Request_endpoint
      with type request = req
       and type response = res ) handler =
  let handler request =
    let%await body = Dream.body request in
    let%await request = Parallel.decode E.request_of_yojson body in
    Metrics.Networking.inc_network_messages_received E.path ;
    Metrics.Networking.measure_network_received_message_size E.path
      (String.length body) ;
    match request with
    | Ok request -> (
        let response = handler request in
        match response with
        | Ok response ->
            let%await response =
              Parallel.encode E.response_to_yojson response
            in
            Dream.json response
        | Error err ->
            raise (Failure (Flows.string_of_error err)) )
    | Error err ->
        raise (Failure err)
  in
  Dream.post E.path handler

(* POST /append-block-and-signature *)
(* If the block is not already known and is valid, add it to the pool *)
let handle_received_block =
  handle_request
    (module Network.Block_spec)
    (fun request ->
      Flows.received_block request.block ;
      Ok () )

(* POST /append-signature *)
(* Append signature to an already existing block? *)
let handle_received_signature =
  handle_request
    (module Network.Signature_spec)
    (fun request ->
      Flows.received_signature ~hash:request.hash ~signature:request.signature ;
      Ok () )

(* POST /block-by-hash *)
(* Retrieve block by provided hash *)
let handle_block_by_hash =
  handle_request
    (module Network.Block_by_hash_spec)
    (fun request ->
      let block = Flows.find_block_by_hash (Server.get_state ()) request.hash in
      Ok block )

(* POST /block-level *)
(* Retrieve height of the chain *)
let handle_block_level =
  handle_request
    (module Network.Block_level)
    (fun () -> Ok {level= Flows.find_block_level (Server.get_state ())})

(* POST /block-by-level *)
(* Retrieves the block at the given level if it exists *)
let handle_block_by_level =
  handle_request
    (module Network.Block_by_level_spec)
    (fun request ->
      let state = Server.get_state () in
      let block_and_timestamp =
        List.find_opt
          (fun (_, block) ->
            Int64.equal block.Protocol.Block.block_height request.level )
          state.applied_blocks
        |> Option.map (fun (timestamp, block) ->
               Network.Block_by_level_spec.{timestamp; block} )
      in
      Ok block_and_timestamp )

(* POST /user_operation_was_included_in_block *)
(* Returns the block height of the first block where an operation
   is included. We only iterate through blocks we haven't seen yet
*)
let handle_user_operation_was_included_in_block =
  handle_request
    (module Network.Block_user_operation_was_included)
    (fun request ->
      let open Protocol in
      (* pass a requested hash and a block height,
         return a block height option, and a block height *)
      let state = Server.get_state () in
      let filtered_list =
        let rec go filtered_list applied_blocks =
          match applied_blocks with
          | [] ->
              filtered_list
          | (time, block) :: tl ->
              if block.Protocol.Block.block_height = request.previous_level then
                filtered_list
              else go ((time, block) :: filtered_list) tl
        in
        List.rev @@ go [] state.applied_blocks
      in
      let new_level = (snd (List.hd @@ state.applied_blocks)).block_height in
      Format.eprintf "halfway\n%!" ;
      let block_height_opt =
        let rec go filtered_list hashes_present =
          match filtered_list with
          | [] -> (
            match List.rev hashes_present with
            | [] ->
                None
            | height :: _ ->
                Some height )
          | (_, block) :: tl ->
              let user_operations = Block.parse_user_operations block in
              if
                List.exists
                  (fun operation ->
                    Crypto.BLAKE2B.equal
                      operation.Protocol.Operation.Core_user.hash
                      request.operation_hash )
                  user_operations
              then go tl (block.Block.block_height :: hashes_present)
              else go tl hashes_present
        in
        go filtered_list []
      in
      Ok (block_height_opt, new_level) )

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
          { snapshot
          ; additional_blocks= snapshots.additional_blocks
          ; last_block= snapshots.last_block
          ; last_block_signatures=
              Signatures.to_list snapshots.last_block_signatures } )

(* POST /request-nonce *)
(* Unused fow now *)
(* Set a new Number Only Used Once for the selected key *)
(* so that the author can make a proof that he is the owner of the secret and can set the URI *)
let handle_request_nonce =
  handle_request
    (module Network.Request_nonce)
    (fun {uri} ->
      let nonce = Flows.request_nonce (Server.get_state ()) update_state uri in
      Ok {nonce} )

(* POST /register-uri *)
(* Set the provided URI of the validator *)
let handle_register_uri =
  handle_request
    (module Network.Register_uri)
    (fun {uri; signature} ->
      Flows.register_uri (Server.get_state ()) update_state ~uri ~signature )

(* POST /user-operation-gossip *)
(* Propagate user operation (core_user.t) over gossip network *)
let handle_receive_user_operation_gossip =
  handle_request
    (module Network.User_operation_gossip)
    (fun request ->
      Flows.received_user_operation request.user_operation ;
      Ok () )

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
          Flows.received_user_operation operation )
        operations ;
      Ok () )

(* POST /user_operations_noop *)
(* Propagate a batch of empty user_operations over gossip network *)
let handle_receive_user_operations_noop =
  handle_request (module Network.User_operations_noop) (fun _ -> Ok ())

(* POST /consensus-operation-gossip *)
(* Add operation from consensu to pending operations *)
let handle_receive_consensus_operation =
  handle_request
    (module Network.Consensus_operation_gossip)
    (fun request ->
      Flows.received_consensus_operation (Server.get_state ())
        request.consensus_operation request.signature )

(* POST /trusted-validators-membership *)
(* Add or Remove a new trusted validator *)
let handle_trusted_validators_membership =
  handle_request
    (module Network.Trusted_validators_membership_change)
    (fun request ->
      Flows.trusted_validators_membership ~payload:request.payload
        ~signature:request.signature ;
      Ok () )

(* POST /withdraw-proof *)
(* Returns a proof that can be provided to Tezos to fulfill a withdraw *)
let handle_withdraw_proof =
  handle_request
    (module Network.Withdraw_proof)
    (fun {operation_hash} ->
      Ok
        (Flows.request_withdraw_proof (Server.get_state ()) ~hash:operation_hash)
      )

(* POST /ticket-balance *)
(* Returns how much of a ticket a key has *)
let handle_ticket_balance =
  handle_request
    (module Network.Ticket_balance)
    (fun {ticket; address} ->
      let state = Server.get_state () in
      let amount =
        Flows.request_ticket_balance state ~ticket
          ~address:(Core_deku.Address.of_key_hash address)
      in
      Ok {amount} )

let node folder port prometheus_port =
  let node = Node_state.get_initial_state ~folder |> Lwt_main.run in
  Tezos_interop.Consensus.listen_operations node.Node.State.interop_context
    ~on_operation:(fun operation -> Flows.received_tezos_operation operation) ;
  Node.Server.start ~initial:node ;
  Dream.initialize_log ~level:`Warning () ;
  let port =
    match port with
    | Some port ->
        port
    | None ->
        Node.Server.get_port () |> Option.value ~default:4440
  in
  Log.info "Listening on port %d" port ;
  Lwt.all
    [ Dream.serve ~interface:"0.0.0.0" ~port
      @@ Dream.router
           [ handle_block_level
           ; handle_received_block
           ; handle_received_signature
           ; handle_block_by_hash
           ; handle_block_by_level
           ; handle_user_operation_was_included_in_block
           ; handle_protocol_snapshot
           ; handle_request_nonce
           ; handle_register_uri
           ; handle_receive_user_operation_gossip
           ; handle_receive_user_operations_gossip
           ; handle_receive_user_operations_noop
           ; handle_receive_consensus_operation
           ; handle_withdraw_proof
           ; handle_ticket_balance
           ; handle_trusted_validators_membership ]
    ; Prometheus_dream.serve prometheus_port ]
  |> Lwt_main.run |> ignore

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-node"

let () =
  (* This is needed because Dream will initialize logs lazily *)
  Dream.initialize_log ~enable:false ()

let node json_logs style_renderer level folder prometheus_port =
  ( match style_renderer with
  | Some style_renderer ->
      Fmt_tty.setup_std_outputs ~style_renderer ()
  | None ->
      Fmt_tty.setup_std_outputs () ) ;
  Logs.set_level level ;
  ( match json_logs with
  | true ->
      Logs.set_reporter (Json_logs_reporter.reporter Fmt.stdout)
  | false ->
      Logs.set_reporter (Logs_fmt.reporter ()) ) ;
  (* disable all non-deku logs *)
  List.iter
    (fun src ->
      let src_name = Logs.Src.name src in
      if
        (not (String.starts_with ~prefix:"deku" src_name))
        && not (String.equal src_name "application")
      then Logs.Src.set_level src (Some Logs.Error) )
    (Logs.Src.list ()) ;
  node folder prometheus_port

let node =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv
  in
  let json_logs =
    let docv = "Json logs" in
    let doc = "This determines whether logs will be printed in json format." in
    Arg.(value & flag & info ~doc ~docv ["json-logs"])
  in
  let port =
    let docv = "port" in
    let doc = "The port to listen on for incoming messages." in
    let env = Cmd.Env.info "PORT" ~doc in
    Arg.(value & opt (some int) None & info ~doc ~docv ~env ["port"])
  in
  let open Term in
  const node $ json_logs $ Fmt_cli.style_renderer () $ Logs_cli.level ()
  $ folder_node $ port $ Prometheus_dream.opts

let _ = Cmd.eval @@ Cmd.v (Cmd.info "deku-node") node
