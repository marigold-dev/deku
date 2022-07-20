open Helpers
open Cmdliner
open Sandbox_helpers
open Cmdliner_helpers
open Tezos
open Feather

let check_liveness rpc_address =
  let%ok consensus_address = get_contract_address rpc_address "consensus" in
  (* TODO: rewrite this to be part of this module *)
  let consensus_address = Address.to_string consensus_address in
  let%ok _result =
    process "check-liveness" [Uri.to_string rpc_address; consensus_address]
    |> run_res in
  Ok ()

let term =
  let open Term in
  const check_liveness $ rpc_address

let info =
  let doc =
    "Checks that the Deku cluster is producing blocks and posting to the main \
     chain" in
  Cmd.info "check-liveness" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
