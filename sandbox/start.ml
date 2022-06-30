open Helpers
open Feather
open Crypto
open Cmdliner
open Sandbox_helpers

let produce_block mode =
  (match mode with
  | Docker ->
    process "docker"
      [
        "exec";
        "-t";
        "deku-node-0";
        "/bin/deku-node";
        "produce-block";
        "/app/data";
      ]
  | Local -> process "deku-node" ["produce-block"; "data/0"])
  |. grep "block.hash"
  |. sed "block.hash: \\([a-f0-9]*\\)" "\\1"
  |. tr "-d" "\t\n\r"
  |> run_res ~error:"Error in prodduce-block"
  |> Result.map BLAKE2B.of_string
  |> Result.map
       (Option.to_result
          ~none:"cannot deserialize block hash from produce-block")
  |> Result.join

let sign_block mode hash i =
  match mode with
  | Docker ->
    process "docker"
      [
        "exec";
        "-t";
        Format.sprintf "deku-node-%d" i;
        "/bin/deku-node";
        "sign-block";
        "/app/data";
        BLAKE2B.to_string hash;
      ]
  | Local ->
    process "deku-node"
      ["sign-block"; Format.sprintf "data/%i" i; BLAKE2B.to_string hash]

let sign_block mode hash validators =
  validators
  |> List.map (sign_block mode hash)
  |> List.map (Feather.collect_in_background stdout_and_status)
  |> List.map Feather.wait

let start_deku_cluster mode validators =
  (* Step 1: Starts all the nodes only if mode is set to local *)
  print_endline "Starting nodes.";
  let running_nodes =
    match mode with
    | Docker -> []
    | Local ->
      validators
      |> List.map (fun i ->
             let data_folder = Format.sprintf "data/%i" i in
             let prometheus_port = 9000 + i in
             deku_node
               [
                 "start";
                 data_folder;
                 "--listen-prometheus";
                 string_of_int prometheus_port;
               ]
             |> run_in_background) in
  Unix.sleep 3;

  (* Step 2: Manually produce the block
     Produce a block using `deku-node produce-block`
     See deku-node produce-block --help *)
  print_endline "Producing a block.";
  let%ok hash = produce_block mode in
  Unix.sleep 3;

  (* Step 3: Manually sign the block
     Sign the previously produced block using `deku-node sign-block`
     See ./src/bin/deku_node.ml:sign_block *)
  print_endline "Signing the block.";
  let _ = validators |> sign_block mode hash in
  print_endline "Cluster bootstrapped.";
  Ok running_nodes

let start mode nodes =
  let validators = make_validators nodes in
  let%ok processes = start_deku_cluster mode validators in
  let _processes = List.map wait processes in
  Ok ()

open Cmdliner_helpers

let term =
  let open Term in
  const start $ mode $ nodes

let info =
  let doc = "Starts a Deku cluster configured with this script" in
  Cmd.info "start" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
