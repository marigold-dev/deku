open Deku_stdlib
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_gossip
open Deku_constants

type node = {
  pool : Parallel.Pool.t;
  network : Network.t;
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
  let chain, chain_actions =
    Chain.incoming_message ~current ~message node.chain
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

and on_gossip_outcome node ~current ~outcome =
  let gossip, action = Gossip.apply ~outcome node.gossip in
  node.gossip <- gossip;
  match action with
  | Some gossip_action -> handle_gossip_action node ~current ~gossip_action
  | None -> ()

and handle_gossip_action node ~current ~gossip_action =
  let open Gossip in
  match gossip_action with
  | Gossip_apply_and_broadcast { message; raw_message } ->
      let () =
        let (Raw_message { hash; raw_content }) = raw_message in
        let raw_expected_hash = Message_hash.to_b58 hash in
        Network.broadcast ~raw_expected_hash ~raw_content node.network
      in
      on_message node ~current ~message
  | Gossip_fragment { fragment } -> handle_gossip_fragment node ~fragment

and handle_gossip_fragment node ~fragment =
  Lwt.async (fun () ->
      let%await outcome =
        Parallel.async node.pool (fun () -> Gossip.compute fragment)
      in
      let current = current () in
      on_gossip_outcome node ~current ~outcome;
      Lwt.return_unit)

let make ~pool ~identity ~validators ~nodes ~bootstrap_key =
  let network = Network.connect ~nodes in
  let gossip = Gossip.empty in
  let chain = Chain.make ~identity ~validators ~pool ~bootstrap_key in
  let node =
    let trigger_timeout () = () in
    { pool; network; gossip; chain; trigger_timeout }
  in
  let promise = on_timeout node in
  (node, promise)

let listen node ~port =
  let on_message ~raw_expected_hash ~raw_content =
    on_network node ~raw_expected_hash ~raw_content
  in
  Network.listen ~port ~on_message
