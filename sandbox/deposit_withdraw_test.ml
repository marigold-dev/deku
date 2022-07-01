open Helpers
open Cmdliner
open Sandbox_helpers
open Cmdliner_helpers
open Sandbox_flows
open Crypto
open Tezos

let deposit_withdraw_test mode validators rpc_url deku_address deku_secret =
  let%ok consensus_address = get_contract_address rpc_url "consensus" in
  (* bootstrap the cluster *)
  let%ok _ = Start.start_deku_cluster mode validators in

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
  let get_balance () =
    match get_balance deku_address dummy_ticket_address with
    | Error err -> Error err
    | Ok balance -> if balance <> 0 then Ok () else Error "Deposit failed" in
  let%ok _ = retry get_balance in
  print_endline "Deposit is ok.";

  (* Withdraw some tickets *)
  (* let%ok current_known_handle_hashes = get_current_known_handle_hashes dummy_ticket_address consensus_address in  *)
  let%ok current_size = get_big_map_size big_map_id in
  let%ok operation_hash = withdraw "./wallet.json" dummy_ticket_address in

  (* wait for a handle hash to appear in the consensus storage *)
  let get_big_map_size () =
    match get_big_map_size big_map_id with
    | Error err -> Error err
    | Ok big_map_size ->
      if big_map_size <> current_size then
        Ok ()
      else
        Error "Big map wasn't updated" in
  let%ok _ = retry get_big_map_size in
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
  retry withdraw

let deposit_withdraw_test mode nodes =
  let rpc_url = rpc_url mode in
  let validators = make_validators nodes in
  let%ok _result =
    deposit_withdraw_test mode validators rpc_url deku_address deku_secret in
  Ok ()

let term =
  let open Term in
  const deposit_withdraw_test $ mode $ nodes

let info =
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
