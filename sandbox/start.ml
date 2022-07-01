open Helpers
open Feather
open Crypto
open Cmdliner
open Sandbox_helpers

(* FIXME: don't hard-code this *)
let local_validators_option =
  [
    "--validator_uris";
    "http://localhost:4440,http://localhost:4441,http://localhost:4442";
  ]

let produce_block () =
  let%ok output =
    process "deku-node"
      (["produce-block"; "data/0"; "--no-json"; "-v"] @ local_validators_option)
    |. process "sed" ["-n"; "s/.*block.hash: \\([a-f0-9]*\\).*/\\1/p"]
    |> run_res ~error:"Error in produce-block" in
  Format.printf "Hash from produce-block: %s" output;
  match BLAKE2B.of_string output with
  | Some hash -> Ok hash
  | None -> Error "cannot deserialize block hash from produce-block"

let sign_block hash i =
  process "deku-node"
    (["sign-block"; Format.sprintf "data/%i" i; BLAKE2B.to_string hash]
    @ local_validators_option)

let sign_block hash validators =
  validators
  |> List.map (sign_block hash)
  |> List.map (Feather.collect_in_background stdout_and_status)
  |> List.map Feather.wait

let start_deku_cluster validators =
  (* Step 1: Starts all the nodes only if mode is set to local *)
  print_endline "Starting nodes.";
  let running_nodes =
    (* Kills deku-node children at exit. FIXME? not sure if good enough *)
    at_exit (fun () ->
        Format.printf "Sandbox is exiting - killing deku-nodes";
        Unix.kill 0 9);
    validators
    |> Lwt_list.iter_p (fun i ->
           let data_folder = Format.sprintf "data/%i" i in
           let prometheus_port = 9000 + i in
           (* We want to dynamically log the stdout and stderr of each of
              these. Hence we handle with Lwt instead of Feather.
              TODO: implement some kind of tee functionality in Feather
              - that would be way easier to use than this. *)
           let%await status =
             Lwt_process.exec ~stdout:`Keep ~stderr:`Keep
               ( "deku-node",
                 [|
                   "deku-node";
                   "start";
                   data_folder;
                   "--listen-prometheus";
                   string_of_int prometheus_port;
                   (* TODO: we should probably change the verbosity *)
                   "-v";
                 |] ) in
           Format.eprintf "deku-node exited with status ";
           (match status with
           | Unix.WEXITED i -> Format.eprintf "EXITED %d" i
           | Unix.WSTOPPED i -> Format.eprintf "STOPPED %d" i
           | Unix.WSIGNALED i -> Format.eprintf "SIGNALED %d" i);
           Format.eprintf "\n%!";
           await ()) in
  let is_cluster_bootstrapped () =
    Format.printf "Waiting for nodes to come online\n%!";
    let%ok _ =
      curl ["-d"; "null"; "http://localhost:4440/block-level"] |> run_res in
    let%ok _ =
      curl ["-d"; "null"; "http://localhost:4441/block-level"] |> run_res in
    let%ok _ =
      curl ["-d"; "null"; "http://localhost:4442/block-level"] |> run_res in
    Ok () in
  let%ok () = retry is_cluster_bootstrapped in

  (* Step 2: Manually produce the block
     Produce a block using `deku-node produce-block`
     See deku-node produce-block --help *)
  print_endline "Producing a block.";
  let%ok hash = produce_block () in
  Unix.sleep 3;

  (* Step 3: Manually sign the block
     Sign the previously produced block using `deku-node sign-block`
     See ./src/bin/deku_node.ml:sign_block *)
  print_endline "Signing the block.";
  let _ = validators |> sign_block hash in
  print_endline "Cluster bootstrapped.";
  Ok running_nodes

let start nodes =
  let validators = make_validators nodes in
  let%ok running_nodes = start_deku_cluster validators in
  let () = Lwt_main.run running_nodes in
  Ok ()

open Cmdliner_helpers

let term =
  let open Term in
  const start $ nodes

let info =
  let doc = "Starts a Deku cluster configured with this script" in
  Cmd.info "start" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
