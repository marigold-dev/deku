open Deku_stdlib
open Deku_concepts
open Deku_storage
open Deku_chain
include Node

let domains = 8

let main () =
  let port = ref 8080 in
  let storage_file = ref "storage.json" in
  let chain_file = ref "chain.bin" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Listening port number (8080 by default)");
      ( "-s",
        Arg.Set_string storage_file,
        " Storage file (storage.json by default)" );
      ("-c", Arg.Set_string chain_file, " Chain file (chain.bin by default)");
    ]
    ignore "Handle Deku communication. Runs forever.";

  let storage_file = !storage_file in
  let chain_file = !chain_file in

  let pool = Parallel.Pool.make ~domains in
  let%await storage = Storage.Config.read ~file:storage_file in
  let Storage.Config.{ secret; validators; nodes } = storage in

  Parallel.Pool.run pool (fun () ->
      let identity = Identity.make secret in
      let%await chain = Storage.Chain.read ~file:chain_file in
      let chain =
        match chain with
        | Some chain -> chain
        | None -> Chain.make ~identity ~validators
      in
      let () =
        let (Chain { consensus; _ }) = chain in
        let (Consensus { current_block; _ }) = consensus in
        let (Block { level; _ }) = current_block in
        let level = Level.to_n level in
        let level = N.to_z level in
        Format.eprintf "%a\n%!" Z.pp_print level
      in
      let chain_ref = ref chain in
      let dump_loop () =
        let rec loop () =
          let chain = !chain_ref in
          Lwt.finalize
            (fun () -> Storage.Chain.write ~pool ~file:chain_file chain)
            loop
        in
        loop ()
      in
      let dump chain = chain_ref := chain in
      let node, promise = Node.make ~pool ~dump ~chain ~nodes in
      Node.listen node ~port:!port;
      Lwt.async (fun () -> dump_loop ());
      promise)

(* let setup_log ?style_renderer level =
     Fmt_tty.setup_std_outputs ?style_renderer ();
     Logs.set_level (Some level);
     Logs.set_reporter (Logs_fmt.reporter ())

   let () = setup_log Logs.Debug *)

(* handle external failures *)
let () =
  Lwt.async_exception_hook :=
    fun exn -> Format.eprintf "async: %s\n%!" (Printexc.to_string exn)

let () =
  Sys.set_signal Sys.sigpipe
    (Sys.Signal_handle (fun _ -> Format.eprintf "SIGPIPE\n%!"))

let () = Lwt_main.run (main ())
