open Helpers
open Cmdliner
open Sandbox_helpers
open Cmdliner_helpers
open Tezos
open Feather

let load_test () =
  let rpc_url = rpc_url Local in
  let%ok dummy_ticket_address = get_contract_address rpc_url "dummy_ticket" in
  let dummy_ticket_address = Address.to_string dummy_ticket_address in
  let%ok _result =
    process "load-test" ["saturate"; dummy_ticket_address] |> run_res in
  Ok ()

let term =
  let open Term in
  const load_test $ const ()

let info =
  let doc = "Load tests a local running Deku cluster" in
  Cmd.info "load-test" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
