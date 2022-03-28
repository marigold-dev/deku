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
    (module E : Networking.Request_endpoint
      with type request = req
       and type response = res) handler =
  let handler request =
    let%await body = Dream.body request in
    match body |> Yojson.Safe.from_string |> E.request_of_yojson with
    | Ok request -> (
      let response = handler update_state request in
      match response with
      | Ok response ->
        response |> E.response_to_yojson |> Yojson.Safe.to_string |> Dream.json
      | Error err -> raise (Failure (Flows.string_of_error err)))
    | Error err -> raise (Failure err) in
  Dream.post E.path handler

let handle_received_block_and_signature =
  handle_request
    (module Networking.Block_and_signature_spec)
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
let handle_received_signature =
  handle_request
    (module Networking.Signature_spec)
    (fun update_state request ->
      let open Flows in
      let%ok () =
        received_signature (Server.get_state ()) update_state ~hash:request.hash
          ~signature:request.signature
        |> ignore_some_errors in
      Ok ())
let handle_block_by_hash =
  handle_request
    (module Networking.Block_by_hash_spec)
    (fun _update_state request ->
      let block = Flows.find_block_by_hash (Server.get_state ()) request.hash in
      Ok block)
let handle_block_level =
  handle_request
    (module Networking.Block_level)
    (fun _update_state _request ->
      Ok { level = Flows.find_block_level (Server.get_state ()) })
let handle_protocol_snapshot =
  handle_request
    (module Networking.Protocol_snapshot)
    (fun _update_state () ->
      let State.{ snapshots; _ } = Server.get_state () in
      Ok
        {
          snapshot = snapshots.current_snapshot;
          additional_blocks = snapshots.additional_blocks;
          last_block = snapshots.last_block;
          last_block_signatures =
            Signatures.to_list snapshots.last_block_signatures;
        })
let handle_request_nonce =
  handle_request
    (module Networking.Request_nonce)
    (fun update_state { uri } ->
      let nonce = Flows.request_nonce (Server.get_state ()) update_state uri in
      Ok { nonce })
let handle_register_uri =
  handle_request
    (module Networking.Register_uri)
    (fun update_state { uri; signature } ->
      Flows.register_uri (Server.get_state ()) update_state ~uri ~signature)
let handle_receive_user_operation_gossip =
  handle_request
    (module Networking.User_operation_gossip)
    (fun update_state request ->
      Flows.received_user_operation (Server.get_state ()) update_state
        request.user_operation)
let handle_receive_consensus_operation =
  handle_request
    (module Networking.Consensus_operation_gossip)
    (fun update_state request ->
      Flows.received_consensus_operation (Server.get_state ()) update_state
        request.consensus_operation request.signature)
let handle_trusted_validators_membership =
  handle_request
    (module Networking.Trusted_validators_membership_change)
    (fun update_state request ->
      Flows.trusted_validators_membership (Server.get_state ()) update_state
        request)
let handle_withdraw_proof =
  handle_request
    (module Networking.Withdraw_proof)
    (fun _ { operation_hash } ->
      Ok
        (Flows.request_withdraw_proof (Server.get_state ()) ~hash:operation_hash))
let handle_ticket_balance =
  handle_request
    (module Networking.Ticket_balance)
    (fun _update_state { ticket; address } ->
      let state = Server.get_state () in
      let amount =
        Flows.request_ticket_balance state ~ticket
          ~address:(Core.Address.of_key_hash address) in
      Ok { amount })
let node folder =
  let node = Node_state.get_initial_state ~folder |> Lwt_main.run in
  Tezos_interop.Consensus.listen_operations
    ~context:node.Node.State.interop_context ~on_operation:(fun operation ->
      Flows.received_tezos_operation (Server.get_state ()) update_state
        operation);
  Node.Server.start ~initial:node;
  Dream.initialize_log ~level:`Warning ();
  let port = Node.Server.get_port () |> Option.get in
  Dream.run ~interface:"0.0.0.0" ~port
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
       ]
  @@ Dream.not_found

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-node"

let node =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const node $ folder_node
let () = Term.exit @@ Term.eval (node, Term.info "deku-node")
