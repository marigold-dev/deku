open Cmdliner
open Feather
open Helpers
open Crypto

let exits =
  Cmd.Exit.defaults
  @ [Cmd.Exit.info 1 ~doc:"expected failure (might not be a bug)"]

let man = [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

let run_ret cmd =
  match collect status cmd with
  | 0 -> `Ok 0
  | status ->
    `Error
      (false, Format.sprintf "Exited with status %d. See output above." status)

let ret_res res =
  match res with
  | Ok _ -> `Ok 0
  | Error err -> `Error (false, err)

let run_res ?(error = "") cmd =
  match collect stdout_and_status cmd with
  | stdout, 0 -> Ok stdout
  | stdout, _ -> Error (error ^ stdout)

(* parsing *)
type mode =
  | Docker
  | Local

let mode_to_string mode =
  match mode with
  | Docker -> "docker"
  | Local -> "local"

let mode =
  let parser string =
    match string with
    | "docker" -> Ok Docker
    | "local" -> Ok Local
    | _ -> Error (`Msg "The allowed mode is docker or local") in
  let printer fmt mode = Format.fprintf fmt "%s" (mode_to_string mode) in
  let open Arg in
  let docv = "mode" in
  let doc = "The mode of the cluster, it can be docker or local" in
  let mode_parser = conv ~docv (parser, printer) in
  value & opt mode_parser Local & info ["mode"] ~docv ~doc

let nodes =
  let parser string =
    let%ok nodes =
      int_of_string_opt string
      |> Option.to_result ~none:(`Msg "number of nodes has to be a number.")
    in
    let%assert () =
      (`Msg "number of nodes must be a strictly positive number", nodes > 0)
    in
    Ok nodes in
  let printer fmt nodes = Format.fprintf fmt "%i" nodes in
  let open Arg in
  let nodes_parser = conv ~docv:"nodes" (parser, printer) in
  let docv = "nodes" in
  let doc = "The number of nodes you want in your cluster" in
  value & opt nodes_parser 3 & info ["nodes"] ~docv ~doc

type verbosity =
  | Debug
  | Info
  | Warn
  | Error

let verbosity_to_string verbosity =
  match verbosity with
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"

let verbosity =
  let parser string =
    match string with
    | "debug" -> Ok Debug
    | "info" -> Ok Info
    | "warn" -> Ok Warn
    | "error" -> Ok Error
    | _ -> Error (`Msg "verbosity level should be: debug/info/warn/error") in
  let printer fmt verbosity =
    Format.fprintf fmt "%s" (verbosity_to_string verbosity) in
  let open Arg in
  let verbosity_parser = conv ~docv:"verbosity" (parser, printer) in
  let docv = "verbosity" in
  let doc = "The verbosity level of the nodes" in
  value & opt verbosity_parser Debug & info ["verbosity"] ~docv ~doc

(* helpers *)
let deku_node args = process "deku-node" args |> run_in_background

let sleep time = process "sleep" [string_of_float time] |> run

let deku_cli args = process "deku-cli" args

let produce_block mode =
  (match mode with
  | Docker ->
    process "docker"
      [
        "exec";
        "-t";
        "deku-node-0";
        "/bin/deku-cli";
        "produce-block";
        "/app/data";
      ]
  | Local -> process "deku-cli" ["produce-block"; "data/0"])
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
  (match mode with
  | Docker ->
    process "docker"
      [
        "exec";
        "-t";
        Format.sprintf "deku-node-%d" i;
        "/bin/deku-cli";
        "sign-block";
        "/app/data";
        BLAKE2B.to_string hash;
      ]
  | Local ->
    process "deku-cli"
      ["sign-block"; Format.sprintf "data/%i" i; BLAKE2B.to_string hash])
  |> run_res ~error:"Error in sign block"

let rm_dir directory = process "rm" ["-rf"; directory] |> run

let ligo args = process "ligo" args

let make_validators nodes = List.init nodes (fun i -> i)

(* start *)
let start_deku_cluster mode validators verbosity =
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
                 data_folder;
                 "--verbosity";
                 verbosity_to_string verbosity;
                 "--listen-prometheus";
                 string_of_int prometheus_port;
               ]) in
  sleep 3.0;

  (* Step 2: Manually produce the block
     Produce a block using `deku-cli produce-block`
     See deku-cli produce-block --help *)
  print_endline "Producing a block.";
  let%ok hash = produce_block mode in
  sleep 3.0;

  (* Step 3: Manually sign the block
     Sign the previously produced block using `deku-cli sign-block`
     See ./src/bin/deku_cli.ml:sign_block *)
  print_endline "Signing the block.";
  let _ = validators |> List.map (sign_block mode hash) in
  print_endline "Cluster bootstrapped.";
  Ok running_nodes

let start mode nodes verbosity =
  let validators = make_validators nodes in
  start_deku_cluster mode validators verbosity
  |> Result.map (List.map wait)
  |> ret_res

let start =
  let open Term in
  const start $ mode $ nodes $ verbosity |> ret

let info_start =
  let doc = "Starts a Deku cluster configured with this script" in
  Cmd.info "start" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* tear-down *)
let tear_down nodes =
  make_validators nodes
  |> List.map (fun i -> Format.sprintf "data/%i" i)
  |> List.iter rm_dir;
  `Ok 0

let tear_down =
  let open Term in
  const tear_down $ nodes |> ret

let info_tear_down =
  let doc = "Stops the Tezos node and destroys the Deku state." in
  Cmd.info "tear-down" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* setup *)
let setup mode nodes =
  process "./sandbox.sh" ["setup"; mode_to_string mode; string_of_int nodes]
  |> run_ret

let setup =
  let open Term in
  const setup $ mode $ nodes |> ret

let info_setup =
  let doc =
    "Does the following: it starts a Tezos sandbox network with Flextesa, then \
     it generates a new validator indentities and it deploys a new contract to \
     the Tezos sandbox configured to use these validators." in
  Cmd.info "setup" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* smoke test *)
let smoke_test mode nodes =
  process "./sandbox.sh"
    ["smoke-test"; mode_to_string mode; string_of_int nodes]
  |> run_ret

let smoke_test =
  let open Term in
  const smoke_test $ mode $ nodes |> ret

let info_smoke_test =
  let doc =
    "Starts a Deku cluster and performs some simple checks that its working."
  in
  Cmd.info "smoke-test" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* deposit-withdraw-test *)
let deposit_withdraw_test mode nodes =
  process "./sandbox.sh"
    ["deposit-withdraw-test"; mode_to_string mode; string_of_int nodes]
  |> run_ret

let deposit_withdraw_test =
  let open Term in
  const deposit_withdraw_test $ mode $ nodes |> ret

let info_deposit_withdraw_test =
  let doc =
    "Start a Deku cluster and originate a dummy tickets and performs a deposit \
     and a withdraw." in
  Cmd.info "deposit-withdraw-test" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man

(* deploy-dummy-ticket *)
let deploy_dummy_ticket mode =
  process "./sandbox.sh" ["deploy-dummy-ticket"; mode_to_string mode] |> run_ret

let deploy_dummy_ticket =
  let open Term in
  const deploy_dummy_ticket $ mode |> ret

let info_deploy_dummy_ticket =
  let doc =
    "Deploys a contract that forges dummy tickets and deposits to Deku." in
  Cmd.info "deploy-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

(* deposit-dummy-ticket *)
let deposit_dummy_ticket mode =
  process "./sandbox.sh" ["deposit-dummy-ticket"; mode_to_string mode]
  |> run_ret

let deposit_dummy_ticket =
  let open Term in
  const deposit_dummy_ticket $ mode |> ret

let info_deposit_dummy_ticket =
  let doc = "Executes a deposit of a dummy ticket to Deku." in
  Cmd.info "deposit-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-sandbox"

let default_info =
  let doc =
    "creates, deploys, and starts Deku clusters in a sandbox mode suitable for \
     local development and testnets. BE ADVISED: some of the configuration \
     options used by deku-sandbox are unsafe for production environments. \
     Refer to the production deployment guide." in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-sandbox" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

let _ =
  exit
  @@ Cmd.eval'
  @@ Cmd.group default_info
       [
         Cmd.v info_start start;
         Cmd.v info_setup setup;
         Cmd.v info_smoke_test smoke_test;
         Cmd.v info_tear_down tear_down;
         Cmd.v info_deposit_withdraw_test deposit_withdraw_test;
         Cmd.v info_deploy_dummy_ticket deploy_dummy_ticket;
         Cmd.v info_deposit_dummy_ticket deposit_dummy_ticket;
       ]
