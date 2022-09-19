open Deku_stdlib
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_gossip
open Deku_constants
open Deku_indexer
open Deku_tezos_interop
open Deku_concepts
open Deku_protocol

type node = {
  pool : Parallel.Pool.t;
  network : Network.t;
  (* TODO: there is a better way to do this but this is the quick and lazy way. *)
  indexer : Indexer.t option;
  mutable gossip : Gossip.t;
  mutable chain : Chain.t;
  mutable trigger_timeout : unit -> unit;
}

type t = node

let current () = Timestamp.of_float (Unix.gettimeofday ())

let rec on_network node ~raw_expected_hash ~raw_content =
  let gossip, fragment =
    Gossip.incoming ~raw_expected_hash ~raw_content node.gossip
  in
  node.gossip <- gossip;
  match fragment with
  | Some fragment -> handle_gossip_fragment node ~fragment
  | None -> ()

and on_message node ~current ~message =
  let open Message in
  let (Message { hash = _; content }) = message in
  let chain, chain_actions =
    Chain.incoming_message ~current ~content node.chain
  in
  node.chain <- chain;
  handle_chain_actions node ~chain_actions

and on_timeout node : unit Lwt.t =
  let trigger_timeout_promise, trigger_timeout_resolver = Lwt.wait () in
  let trigger_timeout () =
    match Lwt.wakeup_later trigger_timeout_resolver () with
    | () -> ()
    | exception _exn -> ()
  in
  node.trigger_timeout <- trigger_timeout;
  let%await () =
    Lwt.pick [ Lwt_unix.sleep block_timeout; trigger_timeout_promise ]
  in
  let current = current () in

  (* TODO: clean in the future *)
  node.gossip <- Gossip.clean ~current:(Timestamp.to_float current) node.gossip;

  let chain, chain_actions = Chain.incoming_timeout ~current node.chain in
  node.chain <- chain;
  handle_chain_actions node ~chain_actions;
  on_timeout node

and handle_chain_actions node ~chain_actions =
  List.iter
    (fun chain_action -> handle_chain_action node ~chain_action)
    chain_actions

and handle_chain_action node ~chain_action =
  let open Chain in
  match chain_action with
  | Chain_trigger_timeout -> node.trigger_timeout ()
  | Chain_broadcast { content } ->
      let fragment = Gossip.broadcast ~content in
      handle_gossip_fragment node ~fragment
  | Chain_save_block block -> (
      match node.indexer with
      | Some indexer -> Indexer.save_block ~block indexer
      | None -> ())

and on_gossip_outcome node ~current ~outcome =
  let gossip, action =
    Gossip.apply ~current:(Timestamp.to_float current) ~outcome node.gossip
  in
  node.gossip <- gossip;
  match action with
  | Some gossip_action -> handle_gossip_action node ~current ~gossip_action
  | None -> ()

and handle_gossip_action node ~current ~gossip_action =
  let open Gossip in
  match gossip_action with
  | Gossip_apply_and_broadcast { message; raw_message } -> (
      let () =
        let (Raw_message { hash; raw_content }) = raw_message in
        let raw_expected_hash = Message_hash.to_b58 hash in
        Network.broadcast ~raw_expected_hash ~raw_content node.network
      in
      let () = on_message node ~current ~message in
      match node.indexer with
      | Some indexer -> Indexer.save_message ~message:raw_message indexer
      | None -> ())
  | Gossip_fragment { fragment } -> handle_gossip_fragment node ~fragment

and handle_gossip_fragment node ~fragment =
  Lwt.async (fun () ->
      let%await outcome =
        Parallel.async node.pool (fun () -> Gossip.compute fragment)
      in
      let current = current () in
      on_gossip_outcome node ~current ~outcome;
      Lwt.return_unit)

(* TODO: declare this function elsewhere ? *)
let to_tezos_operation transaction =
  let open Deku_tezos_interop.Tezos_interop.Consensus in
  let open Deku_protocol in
  match transaction with
  | Deposit { ticket; amount; destination } ->
      N.of_z amount |> Option.map Amount.of_n
      |> Option.map (fun amount ->
             Tezos_operation.Deposit { ticket; amount; destination })
  | _ -> None

let handle_tezos_operation node ~operation =
  let Tezos_interop.Consensus.{ hash; transactions } = operation in
  let operations = List.filter_map to_tezos_operation transactions in
  let tezos_operation = Tezos_operation.make hash operations in
  let chain, chain_actions =
    Chain.incoming_tezos_operation ~tezos_operation node.chain
  in
  node.chain <- chain;
  handle_chain_actions ~chain_actions node

let make ~pool ~identity ~validators ~nodes ~bootstrap_key ?(indexer = None)
    ~default_block_size () =
  let network = Network.connect ~nodes in
  let gossip = Gossip.empty in
  let chain =
    Chain.make ~identity ~validators ~pool ~bootstrap_key ~default_block_size
  in
  let node =
    let trigger_timeout () = () in
    { pool; network; gossip; chain; trigger_timeout; indexer }
  in
  let promise = on_timeout node in
  (node, promise)

let listen node ~port ~tezos_interop =
  let on_message ~raw_expected_hash ~raw_content =
    on_network node ~raw_expected_hash ~raw_content
  in
  Network.listen ~port ~on_message;
  let open Deku_tezos_interop in
  Tezos_interop.Consensus.listen_operations tezos_interop
    ~on_operation:(fun operation -> handle_tezos_operation node ~operation)

let _test () =
  let open Deku_concepts in
  let open Deku_crypto in
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  let identity = Identity.make secret in
  let validators =
    let key = Key.of_secret secret in
    let key_hash = Key_hash.of_key key in
    [ key_hash ]
  in
  let pool = Parallel.Pool.make ~domains:8 in

  let node, promise =
    make ~pool ~identity ~validators ~nodes:[]
      ~bootstrap_key:(Identity.key identity) ~default_block_size:0 ~indexer:None
      ()
  in
  let (Chain { consensus; _ }) = node.chain in
  let block =
    let (Consensus.Consensus { state; _ }) = consensus in
    let (State { current_level; current_block; _ }) = state in
    let level = Level.next current_level in
    let previous = current_block in
    let operations = [] in
    let tezos_operations = [] in
    let block =
      Block.produce ~parallel_map:List.map ~identity ~level ~previous
        ~operations ~tezos_operations
    in
    block
  in

  let () =
    let _message, raw_message =
      Message.encode ~content:(Message.Content.block block)
    in
    let (Raw_message { hash; raw_content }) = raw_message in
    let raw_expected_hash = Message_hash.to_b58 hash in
    on_network node ~raw_expected_hash ~raw_content
  in

  let () =
    let signature = Block.sign ~identity block in
    let _message, raw_message =
      Message.encode ~content:(Message.Content.signature signature)
    in
    let (Raw_message { hash; raw_content }) = raw_message in
    let raw_expected_hash = Message_hash.to_b58 hash in
    on_network node ~raw_expected_hash ~raw_content
  in
  Lwt_main.run promise
