open Helpers
open Cmdliner
open Sandbox_helpers
open Cmdliner_helpers
open Sandbox_flows

let deposit_dummy_ticket rpc_address =
  let%ok _result = deposit_ticket rpc_address deku_address in
  Ok ()

let term =
  let open Term in
  const deposit_dummy_ticket $ rpc_address

let info =
  let doc = "Executes a deposit of a dummy ticket to Deku." in
  Cmd.info "deposit-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man
