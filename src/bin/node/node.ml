open Deku_stdlib
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_gossip
open Deku_constants

type node = {
  pool : Parallel.Pool.t;
  dump : Chain.t -> unit;
  network : Network.t;
  mutable chain : Chain.t;
  mutable trigger_timeout : unit -> unit;
}

type t = node

let current () = Timestamp.of_float (Unix.gettimeofday ())

let write_chain ~chain node =
  node.dump chain;
  node.chain <- chain

let rec on_network_message node ~raw_expected_hash ~raw_content =
  let chain, fragment =
    Chain.incoming ~raw_expected_hash ~raw_content node.chain
  in
  write_chain ~chain node;
  match fragment with
  | Some fragment -> handle_chain_fragment node ~fragment
  | None -> ()

and on_network_request node ~id ~raw_expected_hash ~raw_content =
  let chain, fragment =
    Chain.request ~id ~raw_expected_hash ~raw_content node.chain
  in
  write_chain ~chain node;
  match fragment with
  | Some fragment -> handle_chain_fragment node ~fragment
  | None -> ()

and on_network_response node ~raw_expected_hash ~raw_content =
  let chain, fragment =
    Chain.response ~raw_expected_hash ~raw_content node.chain
  in
  write_chain ~chain node;
  match fragment with
  | Some fragment -> handle_chain_fragment node ~fragment
  | None -> ()

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

  let fragment = Chain.timeout ~current node.chain in
  (match fragment with
  | Some fragment -> handle_chain_fragment node ~fragment
  | None -> ());
  on_timeout node

and on_chain_outcome node ~current ~outcome =
  let chain, actions = Chain.apply ~current ~outcome node.chain in
  write_chain ~chain node;
  handle_chain_actions node ~actions

and handle_chain_actions node ~actions =
  List.iter (fun action -> handle_chain_action node ~action) actions

and handle_chain_action node ~action =
  let open Chain in
  match action with
  | Chain_trigger_timeout -> node.trigger_timeout ()
  | Chain_broadcast { raw_expected_hash; raw_content } ->
      Network.broadcast ~raw_expected_hash ~raw_content node.network
  | Chain_send_request { raw_expected_hash; raw_content } ->
      Lwt.async (fun () ->
          (* TODO: this is non ideal *)
          let%await raw_expected_hash, raw_content =
            Network.request ~raw_expected_hash ~raw_content node.network
          in
          on_network_response node ~raw_expected_hash ~raw_content;
          Lwt.return_unit)
  | Chain_send_response { id; raw_expected_hash; raw_content } ->
      Network.respond ~id ~raw_expected_hash ~raw_content node.network
  | Chain_send_not_found { id } -> Network.not_found ~id node.network
  | Chain_fragment { fragment } -> handle_chain_fragment node ~fragment

and handle_chain_fragment node ~fragment =
  Lwt.async (fun () ->
      let%await outcome =
        Parallel.async node.pool (fun () -> Chain.compute fragment)
      in
      let current = current () in
      on_chain_outcome node ~current ~outcome;
      Lwt.return_unit)

let make ~pool ~dump ~chain ~nodes =
  let network = Network.connect ~nodes in
  let node =
    let trigger_timeout () = () in
    { pool; dump; network; chain; trigger_timeout }
  in
  let promise = on_timeout node in
  (node, promise)

let listen node ~port =
  let on_message ~raw_expected_hash ~raw_content =
    on_network_message node ~raw_expected_hash ~raw_content
  in
  let on_request ~id ~raw_expected_hash ~raw_content =
    on_network_request node ~id ~raw_expected_hash ~raw_content
  in
  Network.listen ~port ~on_message ~on_request node.network

let test () =
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
  let nodes =
    let uri = Uri.of_string "http://localhost:1234" in
    [ uri ]
  in
  let pool = Parallel.Pool.make ~domains:8 in
  let chain = Chain.make ~identity ~validators in
  let dump _chain = () in
  let node, promise = make ~pool ~dump ~chain ~nodes in
  let (Chain { consensus; _ }) = node.chain in
  let block =
    let (Consensus { current_block; _ }) = consensus in
    let (Block { hash = current_block; level = current_level; _ }) =
      current_block
    in
    let level = Level.next current_level in
    let previous = current_block in
    let operations = [] in
    let block = Block.produce ~identity ~level ~previous ~operations in
    block
  in

  let () =
    let _message, raw_message =
      Message.encode ~content:(Message.Content.block block)
    in
    let (Raw_message { hash; raw_content }) = raw_message in
    let raw_expected_hash = Message_hash.to_b58 hash in
    on_network_message node ~raw_expected_hash ~raw_content
  in

  let () =
    let vote = Block.sign ~identity block in
    let _message, raw_message =
      Message.encode ~content:(Message.Content.vote vote)
    in
    let (Raw_message { hash; raw_content }) = raw_message in
    let raw_expected_hash = Message_hash.to_b58 hash in
    on_network_message node ~raw_expected_hash ~raw_content
  in
  Lwt_main.run promise
