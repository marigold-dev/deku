open Cmdliner
open Sandbox_helpers.Cmdliner_helpers

let tear_down =
  let open Term in
  const tear_down $ nodes

let info_tear_down =
  let doc = "Stops the Tezos node and destroys the Deku state." in
  Cmd.info "tear-down" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let deploy_dummy_ticket =
  let open Term in
  const deploy_dummy_ticket $ mode

let info_deploy_dummy_ticket =
  let doc =
    "Deploys a contract that forges dummy tickets and deposits to Deku." in
  Cmd.info "deploy-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

let deposit_dummy_ticket =
  let open Term in
  const deposit_dummy_ticket $ mode

let info_deposit_dummy_ticket =
  let doc = "Executes a deposit of a dummy ticket to Deku." in
  Cmd.info "deposit-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-sandbox"

let default_info =
  let doc =
    "creates, deploys, and starts Deku clusters in a sandbox mode suitable for \
     local development and testnets. BE ADVISED: some of the configuration \
     options used by deku-sandbox are unsafe for production environments. \
     Refer to the production deployment guide." in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-sandbox" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

let _ =
  Cli.make ~info:default_info ()
  |> Cli.add (module Start)
  |> Cli.add (module Setup)
  |> Cli.add (module Teardown)
  |> Cli.add (module Deposit_withdraw_test)
  |> Cli.add (module Deploy_dummy_ticket)
  |> Cli.add (module Deposit_dummy_ticket)
  |> Cli.add (module Load_test)
  |> Cli.add (module Check_liveness)
  |> Cli.eval
