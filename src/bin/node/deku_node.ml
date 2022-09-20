open Deku_stdlib
open Deku_concepts
open Deku_storage
open Deku_chain
include Node

let domains = 8

let make_dump_loop ~pool ~folder ~chain =
  let chain_ref = ref chain in
  let dump_loop () =
    let rec loop () =
      let chain = !chain_ref in
      let%await () =
        Lwt.catch
          (fun () -> Storage.Chain.write ~pool ~folder chain)
          (fun exn ->
            Format.eprintf "storage.failure: %s\n%!" (Printexc.to_string exn);
            Lwt.return_unit)
      in
      loop ()
    in
    loop ()
  in
  let dump chain = chain_ref := chain in
  Lwt.async (fun () -> dump_loop ());
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
  let%await storage = Storage.Config.read ~folder:data_folder in
  let Storage.Config.{ secret; validators; nodes } = storage in

  Parallel.Pool.run pool (fun () ->
      let identity = Identity.make secret in
      let%await chain = Storage.Chain.read ~folder:data_folder in
      let chain =
        match chain with
        | Some chain -> Chain.clear chain
        | None -> Chain.make ~identity ~validators
      in

      let dump = make_dump_loop ~pool ~folder:data_folder ~chain in
      let node, promise = Node.make ~pool ~dump ~chain ~nodes in
      Node.listen node ~port:!port;
      let () =
        let (Chain { consensus; _ }) = chain in
        let (Consensus { current_block; _ }) = consensus in
        let (Block { level; _ }) = current_block in
        let level = Level.to_n level in
        let level = N.to_z level in
        Format.eprintf "%a\n%!" Z.pp_print level
      in
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
