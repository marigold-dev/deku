open Helpers
open Cmdliner
open Sandbox_benchmarks_helpers
open Cmdliner_helpers
open Tezos
open Feather

let network_msg rpc_address =
  let%ok dummy_ticket_address =
    get_contract_address rpc_address "dummy_ticket" in
  let dummy_ticket_address = Address.to_string dummy_ticket_address in
  let%ok _result = process "network-msg" [dummy_ticket_address] |> run_res in
  Ok ()

let term =
  let open Term in
  const network_msg $ rpc_address

let info =
  let doc = "Load tests with noop transaction on a local running Deku cluster" in
  Cmd.info "network-msg" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
