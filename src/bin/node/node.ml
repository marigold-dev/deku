open Deku_stdlib
open Deku_concepts
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_gossip

type node = {
  identity : Identity.t;
  pool : Parallel.Pool.t;
  dump : Chain.t -> unit;
  network : Network_manager.t;
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
  | Chain_broadcast { raw_expected_hash; raw_content } ->
      Network_manager.broadcast ~raw_expected_hash ~raw_content node.network
  | Chain_send_message { connection; raw_expected_hash; raw_content } ->
      Network_manager.send ~connection ~raw_expected_hash ~raw_content
        node.network
  | Chain_send_request { raw_expected_hash; raw_content } ->
      Network_manager.request ~raw_expected_hash ~raw_content node.network
  | Chain_fragment { fragment } -> handle_chain_fragment ~sw ~env ~fragment node

and handle_chain_fragment ~sw ~env ~fragment node =
  Eio.Fiber.fork_sub ~sw ~on_error:Deku_constants.async_on_error (fun sw ->
      let identity = node.identity in
      let outcome =
        Parallel.async node.pool (fun () -> Chain.compute ~identity fragment)
      in
      let outcome = Eio.Promise.await outcome in
      let current = current () in
      on_chain_outcome ~sw ~env ~current ~outcome node)

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

let on_network_message ~sw ~env ~raw_expected_hash ~raw_content node =
  let chain, fragment =
    Chain.incoming ~raw_expected_hash ~raw_content node.chain
  in
  write_chain ~chain node;
  match fragment with
  | Some fragment -> handle_chain_fragment ~sw ~env ~fragment node
  | None -> ()

let on_network_request ~sw ~env ~connection ~raw_expected_hash ~raw_content node
    =
  let fragment = Chain.request ~connection ~raw_expected_hash ~raw_content in
  match fragment with
  | Some fragment -> handle_chain_fragment ~sw ~env ~fragment node
  | None -> ()

let make ~identity ~pool ~dump ~chain =
  let network = Network_manager.make () in
  let cancel () = () in
  { identity; pool; dump; network; chain; cancel }

let on_network_message ~sw ~env ~raw_expected_hash ~raw_content node =
  (* bench "message" @@ fun () -> *)
  on_network_message ~sw ~env ~raw_expected_hash ~raw_content node

let start ~sw ~env ~port ~nodes node =
  let on_connection ~connection =
    let (Chain { consensus; _ }) = node.chain in
    let (Block { level; _ }) = Consensus.trusted_block consensus in
    (* TODO: attack, reconnect, this should be in domain *)
    let content = Request.Content.accepted ~above:level in
    let _request, raw_request = Request.encode ~content in
    let (Raw_request { hash; raw_content }) = raw_request in
    let raw_expected_hash = Request_hash.to_b58 hash in
    Network_manager.send_request ~connection ~raw_expected_hash ~raw_content
      node.network
  in
  let on_message ~raw_expected_hash ~raw_content =
    (* Format.eprintf "incoming(%.3f): %s\n%!" (Unix.gettimeofday ())
       raw_expected_hash; *)
    on_network_message ~sw ~env ~raw_expected_hash ~raw_content node
  in
  let on_request ~connection ~raw_expected_hash ~raw_content =
    on_network_request ~sw ~env ~connection ~raw_expected_hash ~raw_content node
  in

  let () =
    let (Chain { consensus; _ }) = node.chain in
    let current = current () in
    let until = Consensus.next_timeout ~current consensus in
    start_timeout ~sw ~env ~until node
  in
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  Eio.Fiber.both
    (fun () ->
      Network_manager.listen ~net ~clock ~port ~on_connection ~on_message
        ~on_request node.network)
    (fun () ->
      Network_manager.connect ~net ~clock ~nodes ~on_connection ~on_message
        ~on_request node.network)

let test () =
  let pool = Parallel.Pool.make ~domains:8 in
  Parallel.Pool.run pool @@ fun () ->
  Eio_main.run @@ fun env ->
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
      (identity (), 4442);
      (identity (), 4443);
    ]
  in
  let nodes =
    List.map (fun (_identity, port) -> ("localhost", port)) identities
  in
  let validators =
    List.map (fun (identity, _port) -> Identity.key_hash identity) identities
  in

  let start ~sw ~identity ~port =
    let chain = Chain.make ~validators in
    let dump _chain = () in
    let node = make ~identity ~pool ~dump ~chain in
    start ~sw ~env ~port ~nodes node
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
    let network = Network_manager.make () in
    Eio.Time.sleep clock 0.2;
    let () =
      Eio.Fiber.fork ~sw @@ fun () ->
      Network_manager.connect ~net ~clock ~nodes
        ~on_connection:(fun ~connection:_ -> ())
        ~on_request:(fun ~connection:_ ~raw_expected_hash:_ ~raw_content:_ ->
          ())
        ~on_message:(fun ~raw_expected_hash:_ ~raw_content:_ -> ())
        network
    in
    Eio.Time.sleep clock 0.2;
    let broadcast ~content =
      let open Message in
      let _message, raw_message = Message.encode ~content in
      let (Raw_message { hash; raw_content }) = raw_message in
      let raw_expected_hash = Message_hash.to_b58 hash in
      Network_manager.broadcast ~raw_expected_hash ~raw_content network
    in

    let (Block.Block { hash = current_block; level = current_level; _ }) =
      Genesis.block
    in
    let level = Level.next current_level in
    let previous = current_block in
    let operations = [] in
    let block = Block.produce ~identity ~level ~previous ~operations in
    let votes =
      List.map (fun (identity, _port) -> Block.sign ~identity block) identities
    in
    broadcast ~content:(Message.Content.block block);
    List.iter
      (fun vote -> broadcast ~content:(Message.Content.vote ~level ~vote))
      votes
  in

  Eio.Switch.run @@ fun _sw ->
  Eio.Fiber.both (fun () -> start_nodes ()) (fun () -> bootstrap ())
