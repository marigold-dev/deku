open Deku_stdlib
open Deku_concepts
open Deku_storage
open Deku_chain

let domains = 8

let make_dump_loop ~sw ~env ~folder ~chain =
  let chain_ref = ref chain in

  let domains = Eio.Stdenv.domain_mgr env in

  let rec loop () : unit =
    let chain = !chain_ref in
    (try Storage.Chain.write ~env ~folder chain
     with exn ->
       Format.eprintf "storage.failure: %s\n%!" (Printexc.to_string exn));
    loop ()
  in
  let dump chain = chain_ref := chain in
  ( Eio.Fiber.fork_sub ~sw ~on_error:Deku_constants.async_on_error @@ fun _sw ->
    Eio.Domain_manager.run domains (fun () -> loop ()) );
  dump

let main () =
  let port = ref 8080 in
  let data_folder = ref "./data" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Listening port number (8080 by default)");
      ("-d", Arg.Set_string data_folder, " Data folder (./data by default)");
    ]
    ignore "Handle Deku communication. Runs forever.";
  let data_folder = !data_folder in

  let pool = Parallel.Pool.make ~domains in

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Parallel.Pool.run pool @@ fun () ->
  let storage = Storage.Config.read ~env ~folder:data_folder in
  let Storage.Config.{ secret; validators; nodes } = storage in

  let identity = Identity.make secret in
  (* TODO: one problem of loading from disk like this, is that there
       may be pending actions such as fragments being processed *)
  let chain = Storage.Chain.read ~env ~folder:data_folder in
  let chain =
    match chain with
    | Some chain -> Chain.clear chain
    | None -> Chain.make ~identity ~validators
  in

  let dump = make_dump_loop ~sw ~env ~folder:data_folder ~chain in
  let node = Node.make ~pool ~dump ~chain in

  let (Chain { consensus; _ }) = chain in
  let (Block { level; _ }) = Deku_consensus.Consensus.trusted_block consensus in
  let level = Level.to_n level in
  let level = N.to_z level in
  Format.eprintf "%a\n%!" Z.pp_print level;
  Node.start ~sw ~env ~port:!port ~nodes node

(* let setup_log ?style_renderer level =
     Fmt_tty.setup_std_outputs ?style_renderer ();
     Logs.set_level (Some level);
     Logs.set_reporter (Logs_fmt.reporter ())

   let () = setup_log Logs.Debug *)

(* handle external failures *)

(* let _ = main
   let main () = Node.test () *)

let () =
  Sys.set_signal Sys.sigpipe
    (Sys.Signal_handle (fun _ -> Format.eprintf "SIGPIPE\n%!"))

let () = main ()
