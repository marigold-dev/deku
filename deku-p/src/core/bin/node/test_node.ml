open Deku_stdlib
open Deku_chain

let main () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun _sw ->
  Logs_threaded.enable ();
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());

  Parallel.Pool.run ~env ~domains:16 @@ fun () ->
  let open Deku_concepts in
  let open Deku_crypto in
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
    let chain = Chain.make ~validators in
    let dump _chain = () in
    let node =
      Node.make ~identity ~default_block_size:100_000 ~dump ~chain ~indexer:None
    in
    Node.start ~sw ~env ~port ~nodes ~tezos:None node
  in
  let start ~identity ~port =
    Eio.Domain_manager.run domains (fun () ->
        Eio.Switch.run @@ fun sw -> start ~sw ~identity ~port)
  in
  Eio.Fiber.all
    (List.map (fun (identity, port) () -> start ~identity ~port) identities)

let () = main ()
