open Cmdliner
open Feather
open Helpers
open Crypto
open Tezos

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

let fold_results acc =
  List.fold_left
    (fun acc res ->
      match (acc, res) with
      | Ok results, Ok res -> Ok (res :: results)
      | Error err, _ -> Error err
      | Ok _, Error err -> Error err)
    acc

let tezos_client args =
  process "docker"
    (List.append ["exec"; "-t"; "deku_flextesa"; "tezos-client"] args)
  |> run_res ~error:"error in tezos-client"

let rpc_url mode =
  match mode with
  | Docker -> "http://flextesa:20000"
  | Local -> "http://localhost:20000"

let get_contract_address rpc_url contract_name =
  let%ok stdout =
    tezos_client
      ["--endpoint"; rpc_url; "show"; "known"; "contract"; contract_name] in
  stdout
  |> String.split_on_char '\n'
  |> List.filter (String.starts_with ~prefix:"KT1")
  |> (fun list -> List.nth_opt list 0)
  |> Option.map String.trim
  |> Option.map Address.of_string
  |> Option.join
  |> Option.to_result ~none:"Error in contract retrieving"

let deploy_contract rpc_url contract_name contract_path storage wallet =
  Format.printf "Originating new %s contract.@." contract_name;
  let%ok storage =
    ligo ["compile"; "storage"; contract_path; storage]
    |> run_res ~error:"ligo compile storage error" in
  let%ok contract =
    ligo ["compile"; "contract"; contract_path]
    |> run_res ~error:"ligo compile contract error" in
  let%ok _ =
    tezos_client
      [
        "--endpoint";
        rpc_url;
        "originate";
        "contract";
        contract_name;
        "transferring";
        "0";
        "from";
        wallet;
        "running";
        contract;
        "--init";
        storage;
        "--burn-cap";
        "2";
        "--force";
      ] in
  get_contract_address rpc_url contract_name

let deku_address =
  "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf" |> Key_hash.of_string |> Option.get

let deku_secret =
  "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
  |> Secret.of_string
  |> Option.get

let get_deku_height rpc_url =
  let open Yojson.Safe.Util in
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  let consensus_address = Address.to_string consensus_address in
  let url =
    Format.sprintf
      "http://localhost:20000/chains/main/blocks/head/context/contracts/%s/storage"
      consensus_address in
  let%ok stdout = process "curl" ["--silent"; url] |> run_res in
  stdout
  |> Yojson.Safe.from_string
  |> member "args"
  |> to_list
  |> (fun list -> List.nth_opt list 0)
  |> Option.map (member "args")
  |> Option.map to_list
  |> Option.map (fun list -> List.nth_opt list 0)
  |> Option.join
  |> Option.map (member "args")
  |> Option.map to_list
  |> Option.map (fun list -> List.nth_opt list 0)
  |> Option.join
  |> Option.map (member "args")
  |> Option.map to_list
  |> Option.map (fun list -> List.nth_opt list 1)
  |> Option.join
  |> Option.map (member "int")
  |> Option.map Yojson.Safe.Util.to_string_option
  |> Option.join
  |> Option.map int_of_string_opt
  |> Option.join
  |> Option.to_result ~none:"Error in deku-height parsing"

let get_deku_state_hash rpc_url =
  let open Yojson.Safe.Util in
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  let consensus_address = Address.to_string consensus_address in
  let url =
    Format.sprintf "%s/chains/main/blocks/head/context/contracts/%s/storage"
      rpc_url consensus_address in
  let%ok stdout = process "curl" ["--silent"; url] |> run_res in
  stdout
  |> Yojson.Safe.from_string
  |> member "args"
  |> to_list
  |> (fun list -> List.nth_opt list 0)
  |> Option.map (member "args")
  |> Option.map to_list
  |> Option.map (fun list -> List.nth_opt list 0)
  |> Option.join
  |> Option.map (member "args")
  |> Option.map to_list
  |> Option.map (fun list -> List.nth_opt list 2)
  |> Option.join
  |> Option.map (member "bytes")
  |> Option.map to_string_option
  |> Option.join
  |> Option.to_result ~none:"Error in deku-state-hash parsing"

let asserter data_folder state_hash block_height =
  process "asserter" [data_folder; state_hash; block_height]
  |> run_res ~error:"error in asserter"

let killall_deku_nodes () = process "pkill" ["-x"; "deku-node"] |> run

let get_balance address ticketer =
  let ticket = Format.sprintf "(Pair \"%s\" 0x)" (Address.to_string ticketer) in
  deku_cli ["get-balance"; "data/0"; Key_hash.to_string address; ticket]
  |. grep "Balance:"
  |. process "awk" ["{ print $2 }"]
  |> run_res ~error:"error in get balance"
  |> Result.map int_of_string_opt
  |> Result.map (Option.to_result ~none:"error from get-balance parsing")
  |> Result.join

let withdraw path_to_wallet ticketer_address =
  let ticketer_address_string = ticketer_address |> Address.to_string in
  deku_cli
    [
      "withdraw";
      "data/0";
      path_to_wallet;
      ticketer_address_string;
      "10";
      Format.sprintf "Pair \"%s\" 0x" ticketer_address_string;
    ]
  |. process "awk" ["{ print $2 }"]
  |. process "tr" ["-d"; "\t\n\r"]
  |> run_res ~error:"error in withdraw"

let withdraw_proof operation_hash _ticketer_address =
  let open Network in
  let%ok withdraw_proof_response =
    process "curl"
      [
        "-s";
        "-X";
        "POST";
        "-d";
        Format.sprintf "{\"operation_hash\":\"%s\"}" operation_hash;
        operation_hash;
        "http://localhost:4440/withdraw-proof";
      ]
    |> run_res ~error:"error in withdraw-proof" in

  let%ok withdraw_proof_response =
    withdraw_proof_response
    |> Yojson.Safe.from_string
    |> Withdraw_proof.response_of_yojson
    |> Result.map_error (fun _ -> "Error in deserialization") in

  match withdraw_proof_response with
  | Unknown_operation -> Error "unknown operation"
  | Operation_is_not_a_withdraw -> Error "operation is not a withdraw"
  | Ok withdraw_proof ->
    Ok
      ( withdraw_proof.withdrawal_handle.id,
        withdraw_proof.withdrawal_handles_hash,
        withdraw_proof.proof )

let create_wallet address priv_key path =
  let open Infix in
  echo
    (Format.sprintf "{\"address\": \"%s\", \"priv_key\": \"%s\"}"
       (Key_hash.to_string address)
       (Secret.to_string priv_key))
  > path
  |> run_res ~error:"error in create wallet"
  |> Result.map (fun _ -> ())

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

let smoke_test mode validators rpc_url =
  (* bootstrap the network *)
  let%ok _ = start_deku_cluster mode validators Error in
  let%ok starting_height = get_deku_height rpc_url in
  let seconds = 35 in
  sleep (float_of_int seconds);
  let%ok current_state_hash = get_deku_state_hash rpc_url in
  let%ok current_block_height = get_deku_height rpc_url in
  let minimum_expected_height = starting_height + seconds in
  let%assert () =
    ( Format.sprintf "less than 20 blocks were produce in %i seconds." seconds,
      current_block_height - starting_height > 20 ) in
  validators
  |> List.map (fun i ->
         asserter
           (Format.sprintf "data/%i" i)
           current_state_hash
           (string_of_int minimum_expected_height))
  |> fold_results (Ok [])

let smoke_test mode nodes =
  let rpc_url = rpc_url mode in
  let validators = make_validators nodes in
  let test_result = smoke_test mode validators rpc_url in
  killall_deku_nodes ();
  test_result |> ret_res

let smoke_test =
  let open Term in
  const smoke_test $ mode $ nodes |> ret

let info_smoke_test =
  let doc =
    "Starts a Deku cluster and performs some simple checks that its working."
  in
  Cmd.info "smoke-test" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* deploy-dummy-ticket *)
let deploy_dummy_ticket mode =
  deploy_contract (rpc_url mode) "dummy_ticket" "./dummy_ticket.mligo" "()"
    "bob"
  |> Result.map Address.to_string
  |> Result.map print_endline
  |> ret_res

let deploy_dummy_ticket =
  let open Term in
  const deploy_dummy_ticket $ mode |> ret

let info_deploy_dummy_ticket =
  let doc =
    "Deploys a contract that forges dummy tickets and deposits to Deku." in
  Cmd.info "deploy-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

(* deposit-dummy-ticket *)
let deposit_ticket rpc_url deku_address =
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  let consensus_address = Address.to_string consensus_address in
  let input =
    Format.sprintf "Pair (Pair \"%s\" \"%s\") (Pair 100 0x)" consensus_address
      (deku_address |> Key_hash.to_string) in
  tezos_client
    [
      "--endpoint";
      rpc_url;
      "transfer";
      "0";
      "from";
      "bob";
      "to";
      "dummy_ticket";
      "--entrypoint";
      "mint_to_deku";
      "--arg";
      input;
      "--burn-cap";
      "2";
    ]

let deposit_dummy_ticket mode =
  let rpc_url = rpc_url mode in
  deposit_ticket rpc_url deku_address |> ret_res

let deposit_dummy_ticket =
  let open Term in
  const deposit_dummy_ticket $ mode |> ret

let info_deposit_dummy_ticket =
  let doc = "Executes a deposit of a dummy ticket to Deku." in
  Cmd.info "deposit-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

(* deposit-withdraw-test *)
let deposit_withdraw_test mode validators rpc_url deku_address deku_secret =
  (* bootstrap the cluster *)
  let%ok _ = start_deku_cluster mode validators Error in

  (* deploy a dummy ticket *)
  let%ok _ =
    deploy_contract rpc_url "dummy_ticket" "./dummy_ticket.mligo" "()" "bob"
  in

  (* Deposit 100 tickets *)
  let%ok _ = deposit_ticket rpc_url deku_address in
  sleep 10.;

  (* Create a wallet with the deku_address and deku_private key *)
  let%ok () = create_wallet deku_address deku_secret "wallet.json" in

  let%ok dummy_ticket_address = get_contract_address rpc_url "dummy_ticket" in
  let%ok balance = get_balance deku_address dummy_ticket_address in
  let%assert () =
    ( Format.sprintf "Balance for ticket %s is \"%i\"! Did the deposit fail?"
        (dummy_ticket_address |> Address.to_string)
        balance,
      balance <> 0 ) in
  print_endline "Deposit is ok.";

  (* Withdraw some tickets *)
  let%ok operation_hash = withdraw "./wallet.json" dummy_ticket_address in
  sleep 10.0;

  (* Get the proof of the withdraw *)
  let%ok id, handle_hash, proof =
    withdraw_proof operation_hash dummy_ticket_address in
  let handle_hash = BLAKE2B.to_string handle_hash in
  let proof =
    proof
    |> List.map (fun (left, right) ->
           Format.sprintf "Pair 0x%s 0x%s" (BLAKE2B.to_string left)
             (BLAKE2B.to_string right))
    |> String.concat ";" in
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  print_endline "Withdraw-proof is ok.";

  sleep 10.0;

  (* Send the proof to the dummy ticket contract*)
  let arg =
    Format.sprintf
      "Pair (Pair \"%s\" (Pair (Pair (Pair 10 0x) (Pair %d \"%s\")) \"%s\")) \
       (Pair 0x%s {%s})"
      (consensus_address |> Address.to_string)
      id
      (dummy_ticket_address |> Address.to_string)
      (dummy_ticket_address |> Address.to_string)
      handle_hash proof in

  tezos_client
    [
      "transfer";
      "0";
      "from";
      "bob";
      "to";
      "dummy_ticket";
      "--entrypoint";
      "withdraw_from_deku";
      "--arg";
      arg;
      "--burn-cap";
      "2";
    ]

let deposit_withdraw_test mode nodes =
  let rpc_url = rpc_url mode in
  let validators = make_validators nodes in
  let test_result =
    deposit_withdraw_test mode validators rpc_url deku_address deku_secret in
  killall_deku_nodes ();
  test_result |> ret_res

let deposit_withdraw_test =
  let open Term in
  const deposit_withdraw_test $ mode $ nodes |> ret

let info_deposit_withdraw_test =
  let doc = "Tests the deposit/withdraw scenario" in
  let man =
    `S Manpage.s_description
    :: `P
         "Does the following: starts a Deku cluster, originates a dummy ticket \
          contract, deposits some ticket on Deku, checks the balance, requests \
          a withdraw and the associated withdraw proof, sends the withdraw \
          proof to the consensus contract."
    :: man in
  Cmd.info "deposit-withdraw-test" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man

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
