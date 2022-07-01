open Helpers
open Sandbox_helpers
open Feather
open Crypto
open Tezos

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
