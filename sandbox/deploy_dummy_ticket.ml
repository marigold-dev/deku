open Helpers
open Cmdliner
open Sandbox_helpers
open Cmdliner_helpers
open Tezos

let deploy_dummy_ticket () =
  deploy_contract rpc_url "dummy_ticket" "./dummy_ticket.mligo" "()" "bob"
  |> Result.map Address.to_string
  |> Result.map print_endline

let term =
  let open Term in
  const deploy_dummy_ticket $ const ()

let info =
  let doc =
    "Deploys a contract that forges dummy tickets and deposits to Deku." in
  Cmd.info "deploy-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man
