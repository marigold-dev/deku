open Helpers
open Feather
open Crypto
open Tezos
open Protocol

let run_res ?(error = "") cmd =
  match collect stdout_and_status cmd with
  | stdout, 0 -> Ok stdout
  | stdout, _ -> Error (error ^ stdout)

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

let verbosity_of_string = function
  | "debug" -> Ok Debug
  | "info" -> Ok Info
  | "warn" -> Ok Warn
  | "error" -> Ok Error
  | _ -> Error "verbosity level should be: debug/info/warn/error"

type mode =
  | Docker
  | Local

let mode_to_string mode =
  match mode with
  | Docker -> "docker"
  | Local -> "local"

let mode_of_string = function
  | "docker" -> Ok Docker
  | "local" -> Ok Local
  | _ -> Error "The allowed mode is docker or local"

(* helpers *)
let deku_node args = process "deku-node" args

let deku_cli args = process "deku-cli" args

let rm_dir directory = process "rm" ["-rf"; directory] |> run

let ligo args = process "ligo" args

let tezos_client args =
  process "docker"
    (List.append ["exec"; "-t"; "deku_flextesa"; "tezos-client"] args)
  |> run_res ~error:"error in tezos-client"

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

let make_validators nodes = List.init nodes (fun i -> i)

let get_balance address ticketer =
  let ticket = Format.sprintf "(Pair \"%s\" 0x)" (Address.to_string ticketer) in
  deku_cli ["get-balance"; "data/0"; Key_hash.to_string address; ticket]
  |. grep "Balance:"
  |. process "awk" ["{ print $2 }"]
  |> run_res ~error:"error in get balance"
  |> Result.map int_of_string_opt
  |> Result.map (Option.to_result ~none:"error from get-balance parsing")
  |> Result.join

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
  Format.printf "Originating new %s contract." contract_name;
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
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  let consensus_address = Address.to_string consensus_address in
  let url =
    Format.sprintf
      "http://localhost:20000/chains/main/blocks/head/context/contracts/%s/storage"
      consensus_address in
  process "curl" ["--silent"; url]
  |. process "jq" [".args[0].args[0].args[0].args[1].int"]
  |. process "xargs" []
  |> run_res ~error:"get deku-height with tezos-client error"
  |> Result.map int_of_string_opt
  |> Result.map (Option.to_result ~none:"cannot parse output from deku-height")
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

let is_node_bootstrapped rpc_url =
  tezos_client ["--endpoint"; rpc_url; "bootstrapped"]
  |> Result.map (String.split_on_char '\n')
  |> Result.map List.rev
  |> Result.map (fun list -> List.nth_opt list 0)
  |> Result.map
       (Option.to_result ~none:"No output from tezos-client bootstrapped")
  |> Result.join
  |> Result.map (String.starts_with ~prefix:"Node is bootstrapped")
  |> Result.fold ~ok:(fun _ -> true) ~error:(fun _ -> false)

let tezos_client_update_config rpc_url =
  tezos_client ["--endpoint"; rpc_url; "config"; "update"]

let import_secret rpc_url alias secret =
  tezos_client
    ["--endpoint"; rpc_url; "import"; "secret"; "key"; alias; secret; "--force"]

let setup_identity mode i =
  let folder = Format.sprintf "data/%d" i in
  process "mkdir" ["-p"; folder] |> run;
  (match mode with
  | Docker ->
    deku_cli
      [
        "setup-identity";
        folder;
        "--uri";
        Format.sprintf "http://deku-node-%d:4440" i;
      ]
  | Local ->
    deku_node
      [
        "setup-identity";
        folder;
        "--uri";
        Format.sprintf "http://localhost:444%d" i;
      ])
  |> run;
  let%ok key =
    deku_node ["self"; folder]
    |. grep "key:"
    |. process "awk" ["{ print $2 }"]
    |> collect stdout
    |> Wallet.of_string
    |> Option.to_result ~none:"error in key parsing" in
  let%ok address =
    deku_node ["self"; folder]
    |. grep "address"
    |. process "awk" ["{ print $2 }"]
    |> collect stdout
    |> Key_hash.of_string
    |> Option.to_result ~none:"error in address parsing" in
  let uri =
    deku_node ["self"; folder]
    |. grep "uri:"
    |. process "awk" ["{ print $2 }"]
    |> collect stdout in
  Ok (i, key, uri, address)

let make_consensus_storage identities =
  identities
  |> List.map (fun (_, _, _, address) ->
         Format.sprintf "(\"%s\" : key_hash)" (Key_hash.to_string address))
  |> String.concat ";"
  |> Format.sprintf
       "{root_hash = { current_block_hash = 0x;current_block_height = \
        0;current_state_hash = 0x;current_handles_hash = 0x;current_validators \
        = [%s]; }; \n\
       \          vault = {\n\
       \            known_handles_hash = (Big_map.empty : \
        vault_known_handles_hash_set); used_handles = (Big_map.empty : \
        vault_used_handle_set); vault = (Big_map.empty : vault); } }"

let make_discovery_storage identities =
  identities
  |> List.map (fun (_, _, uri, address) ->
         Format.sprintf "((\"%s\" : key_hash), (0, \"%s\"))"
           (Key_hash.to_string address)
           uri)
  |> String.concat ";"
  |> Format.sprintf "Big_map.literal [%s]"

let make_trusted_validator_membership_change_json identities =
  let open Consensus.Trusted_validators_membership_change in
  let json =
    List.map (fun (_, _, _, address) -> { action = Add; address }) identities
    |> [%to_yojson: t list] in
  List.iter
    (fun (i, _, _, _) ->
      let path_to_file =
        Format.sprintf "data/%d/trusted-validator-membership-change.json" i
      in
      Yojson.Safe.to_file path_to_file json)
    identities

let setup_tezos rpc_node tezos_secret consensus_address discovery_address
    identity =
  let i, _, _, _ = identity in
  let folder = Format.sprintf "data/%i" i in
  deku_node
    [
      "setup-tezos";
      folder;
      "--tezos_consensus_contract";
      Address.to_string consensus_address;
      "--tezos_discovery_contract";
      Address.to_string discovery_address;
      "--tezos_rpc_node";
      rpc_node;
      "--tezos_secret";
      tezos_secret;
      "--unsafe_tezos_required_confirmations";
      "4";
    ]
  |> run_res ~error:"error in deku-cli setup-tezos"

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
                 "start";
                 data_folder;
                 "--verbosity";
                 verbosity_to_string verbosity;
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

let start mode nodes verbosity =
  let validators = make_validators nodes in
  let%ok processes = start_deku_cluster mode validators verbosity in
  let _processes = List.map wait processes in
  Ok ()

let tear_down nodes =
  make_validators nodes
  |> List.map (fun i -> Format.sprintf "data/%i" i)
  |> List.iter rm_dir;
  Ok ()

let setup mode validators rpc_url =
  let consensus = "./src/tezos_interop/consensus.mligo" in
  let discovery = "./src/tezos_interop/discovery.mligo" in
  let secret = "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq" in
  validators
  |> List.map (fun i -> Format.sprintf "data/%i" i)
  |> List.iter rm_dir;
  (* setup tezos-client *)
  let%assert () =
    ("the tezos node is not bootstrapped", is_node_bootstrapped rpc_url) in
  let%ok _ = tezos_client_update_config rpc_url in
  let%ok _ =
    import_secret rpc_url "myWallet" (Format.sprintf "unencrypted:%s" secret)
  in

  (* setup write indentity.json to file system *)
  let%ok identities = validators |> List.map_ok (setup_identity mode) in

  (* deploy smart contracts *)
  let consensus_storage = make_consensus_storage identities in
  let discovery_storage = make_discovery_storage identities in
  let%ok consensus_address =
    deploy_contract rpc_url "consensus" consensus consensus_storage "myWallet"
  in
  let%ok discovery_address =
    deploy_contract rpc_url "discovery" discovery discovery_storage "myWallet"
  in

  (* setup tezos informations *)
  make_trusted_validator_membership_change_json identities;
  identities
  |> List.map_ok
       (setup_tezos rpc_url secret consensus_address discovery_address)

let setup mode nodes =
  let validators = make_validators nodes in
  let rpc_url = rpc_url mode in
  let%ok _validators = setup mode validators rpc_url in
  Ok ()

let deploy_dummy_ticket mode =
  deploy_contract (rpc_url mode) "dummy_ticket" "./dummy_ticket.mligo" "()"
    "bob"
  |> Result.map Address.to_string
  |> Result.map print_endline

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
  let%ok _result = deposit_ticket rpc_url deku_address in
  Ok ()

let deposit_withdraw_test mode validators rpc_url deku_address deku_secret =
  (* bootstrap the cluster *)
  let%ok _ = start_deku_cluster mode validators Error in

  (* deploy a dummy ticket *)
  let%ok _ =
    deploy_contract rpc_url "dummy_ticket" "./dummy_ticket.mligo" "()" "bob"
  in

  (* Deposit 100 tickets *)
  let%ok _ = deposit_ticket rpc_url deku_address in
  Unix.sleep 5;

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
  Unix.sleep 10;

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

  Unix.sleep 10;

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
  let%ok _result = test_result in
  Ok ()

let load_test () =
  let rpc_url = rpc_url Local in
  let%ok dummy_ticket_address = get_contract_address rpc_url "dummy_ticket" in
  let dummy_ticket_address = Address.to_string dummy_ticket_address in
  let%ok _result =
    process "load-test" ["saturate"; dummy_ticket_address] |> run_res in
  Ok ()

let check_liveness mode =
  let rpc_url = rpc_url mode in
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  (* TODO: rewrite this to be part of this module *)
  let consensus_address = Address.to_string consensus_address in
  let%ok _result =
    process "check-liveness" [rpc_url; consensus_address] |> run_res in
  Ok ()
