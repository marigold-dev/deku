open Cmdliner

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
  exit
  @@ Cmd.eval_result
  @@ Cmd.group default_info
       [
         Cmd.v Start.info Start.term;
         Cmd.v Setup.info Setup.term;
         Cmd.v Teardown.info Teardown.term;
         Cmd.v Deposit_withdraw_test.info Deposit_withdraw_test.term;
         Cmd.v Deploy_dummy_ticket.info Deploy_dummy_ticket.term;
         Cmd.v Deposit_dummy_ticket.info Deposit_dummy_ticket.term;
         Cmd.v Load_test.info Load_test.term;
         Cmd.v Check_liveness.info Check_liveness.term;
       ]
