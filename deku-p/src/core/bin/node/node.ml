open Deku_stdlib
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_gossip
open Deku_block_storage
open Deku_tezos_interop
open Deku_concepts
open Deku_protocol

type node = {
  identity : Identity.t;
  default_block_size : int;
  dump : Chain.t -> unit;
  network : Network_manager.t;
  (* TODO: there is a better way to do this but this is the quick and lazy way. *)
  indexer : Block_storage.t option;
  (*  TODO: there is a better way to do this but this is the quick and lazy way.  *)
  mutable tezos_interop : Tezos_interop.t option;
  mutable chain : Chain.t;
  mutable cancel : unit -> unit;
}

type t = node

let current () = Timestamp.of_float (Unix.gettimeofday ())

let write_chain ~chain node =
  node.dump chain;
  node.chain <- chain

let rec handle_chain_actions ~sw ~env ~actions node =
  List.iter (fun action -> handle_chain_action ~sw ~env ~action node) actions

and handle_chain_action ~sw ~env ~action node =
  let open Chain in
  match action with
  | Chain_timeout { until } -> start_timeout ~sw ~env ~until node
  | Chain_broadcast { raw_header; raw_content } ->
      Network_manager.broadcast ~raw_header ~raw_content node.network
  | Chain_send_message { connection; raw_header; raw_content } ->
      Network_manager.send ~connection ~raw_header ~raw_content node.network
  | Chain_send_request { raw_header; raw_content } ->
      Network_manager.request ~raw_header ~raw_content node.network
  | Chain_fragment { fragment } -> handle_chain_fragment ~sw ~env ~fragment node
  | Chain_save_block { block } -> (
      match node.indexer with
      | Some indexer -> Block_storage.async_save_block ~sw ~block indexer
      | None -> ())
  | Chain_commit
      {
        current_level;
        payload_hash;
        state_root_hash;
        signatures;
        validators;
        withdrawal_handles_hash;
      } -> (
      match node.tezos_interop with
      | Some tezos_interop ->
          Eio.Fiber.fork ~sw @@ fun () ->
          Tezos_interop.commit_state_hash ~block_level:current_level
            ~block_payload_hash:payload_hash ~state_hash:state_root_hash
            ~withdrawal_handles_hash ~signatures ~validators tezos_interop
      | None -> ())
(* FIXME: this is probably an indication of bad abstraction but being lazy right now *)
(* failwith "Node was not initialized with Tezos interop enabled.") *)

and handle_chain_fragment ~sw ~env ~fragment node =
  Eio.Fiber.fork ~sw @@ fun () ->
  let identity = node.identity in
  let default_block_size = node.default_block_size in
  let outcome =
    Parallel.parallel (fun () ->
        Chain.compute ~identity ~default_block_size fragment)
  in
  let current = current () in
  on_chain_outcome ~sw ~env ~current ~outcome node

and on_chain_outcome ~sw ~env ~current ~outcome node =
  let identity = node.identity in

  let chain, actions = Chain.apply ~identity ~current ~outcome node.chain in
  write_chain ~chain node;
  handle_chain_actions ~sw ~env ~actions node

and start_timeout ~sw ~env ~until node =
  node.cancel ();
  let cancelled = ref false in
  node.cancel <- (fun () -> cancelled := true);

  Eio.Fiber.fork ~sw @@ fun () ->
  let clock = Eio.Stdenv.clock env in
  let () =
    let until = Timestamp.to_float until in
    Eio.Time.sleep_until clock until
  in
  match !cancelled with
  | true -> ()
  | false ->
      let current = current () in
      on_timeout ~sw ~env ~current node

and on_timeout ~sw ~env ~current node =
  let identity = node.identity in
  let chain, actions = Chain.timeout ~identity ~current node.chain in
  write_chain ~chain node;
  handle_chain_actions ~sw ~env ~actions node

let on_network_message ~sw ~env ~raw_header ~raw_content node =
  let chain, fragment = Chain.incoming ~raw_header ~raw_content node.chain in
  write_chain ~chain node;
  match fragment with
  | Some fragment -> handle_chain_fragment ~sw ~env ~fragment node
  | None -> ()

let on_network_request ~sw ~env ~connection ~raw_header ~raw_content node =
  let fragment = Chain.request ~connection ~raw_header ~raw_content in
  handle_chain_fragment ~sw ~env ~fragment node

let make ~identity ~default_block_size ~dump ~chain ~indexer =
  let network = Network_manager.make ~identity in
  let tezos_interop = None in
  let cancel () = () in
  {
    identity;
    default_block_size;
    dump;
    network;
    indexer;
    tezos_interop;
    chain;
    cancel;
  }

(* TODO: declare this function elsewhere ? *)
let to_tezos_operation transaction =
  let open Tezos_interop in
  let open Deku_protocol in
  match transaction with
  | Deposit { ticket; amount; destination } ->
      N.of_z amount |> Option.map Amount.of_n
      |> Option.map (fun amount ->
             Tezos_operation.Deposit { ticket; amount; destination })
  | _ -> None

let handle_tezos_operation ~sw ~env ~operation node =
  let Tezos_interop.{ hash; transactions } = operation in
  let operations = List.filter_map to_tezos_operation transactions in
  let tezos_operation = Tezos_operation.make hash operations in
  let chain, actions =
    Chain.incoming_tezos_operation ~tezos_operation node.chain
  in
  write_chain ~chain node;
  handle_chain_actions ~sw ~env ~actions node

let reload ~sw ~env node =
  let current = current () in
  let chain, actions = Chain.reload ~current node.chain in
  write_chain ~chain node;
  handle_chain_actions ~sw ~env ~actions node

let start ~sw ~env ~port ~nodes ~tezos node =
  let on_connection ~connection =
    let (Chain { consensus; _ }) = node.chain in
    let (Block { level; _ }) = Consensus.trusted_block consensus in
    (* TODO: attack, reconnect, this should be in domain *)
    let (Request { hash = _; above = _; network }) =
      Request.encode ~above:level
    in
    let (Network_request { raw_header; raw_content }) = network in
    Network_manager.send_request ~connection ~raw_header ~raw_content
      node.network
  in
  let on_message ~raw_header ~raw_content =
    (* Logs.info (fun m -> m "incoming(%.3f): %s\n%!" (Unix.gettimeofday ()) raw_header;) *)
    on_network_message ~sw ~env ~raw_header ~raw_content node
  in
  let on_request ~connection ~raw_header ~raw_content =
    on_network_request ~sw ~env ~connection ~raw_header ~raw_content node
  in

  (match tezos with
  | Some (rpc_node, secret, consensus_contract) ->
      let interop =
        Tezos_interop.start ~sw ~rpc_node ~secret ~consensus_contract
          ~on_operation:(fun operation ->
            handle_tezos_operation ~sw ~env ~operation node)
      in
      node.tezos_interop <- Some interop
  | None -> ());
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  Eio.Fiber.all
    [
      (fun () -> reload ~sw ~env node);
      (fun () ->
        Network_manager.listen ~net ~clock ~port ~on_connection ~on_message
          ~on_request node.network);
      (fun () ->
        Network_manager.connect ~net ~clock ~nodes ~on_connection ~on_message
          ~on_request node.network);
    ]

let test () =
  Eio_main.run @@ fun env ->
  Parallel.Pool.run ~env ~domains:16 @@ fun () ->
  let open Deku_concepts in
  let open Deku_crypto in
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let domains = Eio.Stdenv.domain_mgr env in
  let identity () =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    Identity.make secret
  in
  let identities =
    [
      (identity (), 4440);
      (identity (), 4441);
      (* (identity (), 4442);
         (identity (), 4443); *)
    ]
  in

  let nodes =
    List.map (fun (_identity, port) -> ("localhost", port)) identities
  in
  let validators =
    List.map (fun (identity, _port) -> Identity.key_hash identity) identities
  in

  let start ~sw ~identity ~port =
    let chain = Chain.make ~validators ~vm_state:Ocaml_wasm_vm.State.empty in
    let dump _chain = () in
    let node =
      make ~identity ~default_block_size:100_000 ~dump ~chain ~indexer:None
    in
    start ~sw ~env ~port ~nodes ~tezos:None node
  in
  let start ~identity ~port =
    Eio.Domain_manager.run domains (fun () ->
        Eio.Switch.run @@ fun sw -> start ~sw ~identity ~port)
  in
  let start_nodes () =
    Eio.Fiber.all
      (List.map (fun (identity, port) () -> start ~identity ~port) identities)
  in

  let bootstrap () =
    Eio.Switch.run @@ fun sw ->
    let identity, _ = List.nth identities 0 in
    let network = Network_manager.make ~identity in
    Eio.Time.sleep clock 0.2;
    let () =
      Eio.Fiber.fork ~sw @@ fun () ->
      Network_manager.connect ~net ~clock ~nodes
        ~on_connection:(fun ~connection:_ -> ())
        ~on_request:(fun ~connection:_ ~raw_header:_ ~raw_content:_ -> ())
        ~on_message:(fun ~raw_header:_ ~raw_content:_ -> ())
        network
    in
    Eio.Time.sleep clock 0.2;
    let broadcast ~content =
      let open Message in
      let (Message { network = Network_message { raw_header; raw_content }; _ })
          =
        Message.encode ~content
      in
      Network_manager.broadcast ~raw_header ~raw_content network
    in

    let (Block.Block { hash = current_block; level = current_level; _ }) =
      Genesis.block
    in
    let level = Level.next current_level in
    let withdrawal_handles_hash = BLAKE2b.hash "tuturu" in
    let previous = current_block in
    let payload = Payload.Payload [] in
    let tezos_operations = [] in
    let block =
      Block.produce ~identity ~level ~previous ~payload ~tezos_operations
        ~withdrawal_handles_hash
    in
    broadcast ~content:(Message.Content.block block)
  in

  Eio.Switch.run @@ fun _sw ->
  Eio.Fiber.both (fun () -> start_nodes ()) (fun () -> bootstrap ())
