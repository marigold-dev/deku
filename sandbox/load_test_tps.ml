open Helpers
open Cmdliner
open Sandbox_helpers
open Cmdliner_helpers
open Tezos
open Feather

let load_test_tps rpc_address =
  let%ok dummy_ticket_address =
    get_contract_address rpc_address "dummy_ticket" in
  let dummy_ticket_address = Address.to_string dummy_ticket_address in
  let%ok _result =
    process "deku-load-test-tps" [dummy_ticket_address] |> run_res in
  Ok ()

let term =
  let open Term in
  const load_test_tps $ rpc_address

let info =
  let doc = "Load tests a local running Deku cluster" in
  Cmd.info "load-test-tps" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
