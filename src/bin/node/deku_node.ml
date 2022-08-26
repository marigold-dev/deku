open Deku_stdlib
open Deku_concepts
open Deku_storage
include Node

let domains = 8

let main () =
  let port = ref 8080 in
  let storage = ref "storage.json" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Listening port number (8080 by default)");
      ("-s", Arg.Set_string storage, " Storage file (storage.json by default)");
    ]
    ignore "Handle Deku communication. Runs forever.";

  let pool = Parallel.Pool.make ~domains in
  let%await storage = Storage.read ~file:!storage in
  let Storage.{ secret; initial_validators; nodes } = storage in
  let identity = Identity.make secret in
  Parallel.Pool.run pool (fun () ->
      let node, promise =
        Node.make ~pool ~identity ~validators:initial_validators ~nodes
      in
      Node.listen node ~port:!port;
      promise)

(* let setup_log ?style_renderer level =
     Fmt_tty.setup_std_outputs ?style_renderer ();
     Logs.set_level (Some level);
     Logs.set_reporter (Logs_fmt.reporter ())

   let () = setup_log Logs.Debug *)
let () = Lwt_main.run (main ())
