open Helpers
open Sandbox_helpers
open Feather
open Crypto
open Tezos

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

let get_balance address ticketer =
  let ticket = Format.sprintf "(Pair \"%s\" 0x)" (Address.to_string ticketer) in
  deku_cli ["get-balance"; "data/0"; Key_hash.to_string address; ticket]
  |. grep "Balance:"
  |. process "awk" ["{ print $2 }"]
  |> run_res ~error:"error in get balance"
  |> Result.map int_of_string_opt
  |> Result.map (Option.to_result ~none:"error from get-balance parsing")
  |> Result.join

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

(* start *)
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

let tear_down nodes =
  make_validators nodes
  |> List.map (fun i -> Format.sprintf "data/%i" i)
  |> List.iter rm_dir;
  Ok ()

let deploy_dummy_ticket mode =
  deploy_contract (rpc_url mode) "dummy_ticket" "./dummy_ticket.mligo" "()"
    "bob"
  |> Result.map Address.to_string
  |> Result.map print_endline

let deposit_ticket ?(wait = None) rpc_url deku_address =
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  let consensus_address = Address.to_string consensus_address in
  let input =
    Format.sprintf "Pair (Pair \"%s\" \"%s\") (Pair 100 0x)" consensus_address
      (deku_address |> Key_hash.to_string) in
  tezos_client ~wait
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
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  (* bootstrap the cluster *)
  let%ok _ = start_deku_cluster mode validators in

  (* deploy a dummy ticket *)
  let%ok dummy_ticket_address =
    deploy_contract ~wait:(Some 1) rpc_url "dummy_ticket" "./dummy_ticket.mligo"
      "()" "bob" in

  (* Deposit 100 tickets *)
  let%ok _ = deposit_ticket ~wait:(Some 1) rpc_url deku_address in

  (* Create a wallet with the deku_address and deku_private key *)
  let%ok () = create_wallet deku_address deku_secret "wallet.json" in
  (* Retrieves the known handles hash big map id to poll it *)
  let%ok big_map_id = known_handles_hash_big_map_id consensus_address in

  (* Wait for deposit to appear in Deku *)
  let get_balance () = get_balance deku_address dummy_ticket_address in
  let%ok _ =
    poll get_balance (fun balance ->
        if balance <> 0 then Ok balance else Error "Deposit failed") in
  print_endline "Deposit is ok.";

  (* Withdraw some tickets *)
  (* let%ok current_known_handle_hashes = get_current_known_handle_hashes dummy_ticket_address consensus_address in  *)
  let%ok current_size = get_big_map_size big_map_id in
  let%ok operation_hash = withdraw "./wallet.json" dummy_ticket_address in

  (* wait for a handle hash to appear in the consensus storage *)
  let get_big_map_size () = get_big_map_size big_map_id in
  let%ok _ =
    poll get_big_map_size (fun big_map_size ->
        if big_map_size <> current_size then
          Ok ()
        else
          Error "Big map wasn't updated") in
  print_endline "Withdraw is ok.";

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

  let withdraw () =
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
      ] in
  poll withdraw (fun res -> Ok res)

let deposit_withdraw_test mode nodes =
  let rpc_url = rpc_url mode in
  let validators = make_validators nodes in
  let%ok _result =
    deposit_withdraw_test mode validators rpc_url deku_address deku_secret in
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
