open Cmdliner
open Helpers
open Sandbox_flows
open Sandbox_helpers

let exits =
  Cmd.Exit.defaults
  @ [Cmd.Exit.info 1 ~doc:"expected failure (might not be a bug)"]

let man = [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

(* parsing *)
let mode =
  let printer fmt mode = Format.fprintf fmt "%s" (mode_to_string mode) in
  let open Arg in
  let docv = "mode" in
  let doc = "The mode of the cluster, it can be docker or local" in
  let mode_parser = conv' ~docv (mode_of_string, printer) in
  value & opt mode_parser Local & info ["mode"] ~docv ~doc

let nodes =
  let parser string =
    let%ok nodes =
      int_of_string_opt string
      |> Option.to_result ~none:(`Msg "number of nodes has to be a number.")
    in
    let%assert () =
      (`Msg "number of nodes must be a strictly positive number", nodes > 0)
    in
    Ok nodes in
  let printer fmt nodes = Format.fprintf fmt "%i" nodes in
  let open Arg in
  let nodes_parser = conv ~docv:"nodes" (parser, printer) in
  let docv = "nodes" in
  let doc = "The number of nodes you want in your cluster" in
  value & opt nodes_parser 3 & info ["nodes"] ~docv ~doc

let start =
  let open Term in
  const start $ mode $ nodes

let info_start =
  let doc = "Starts a Deku cluster configured with this script" in
  Cmd.info "start" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let tear_down =
  let open Term in
  const tear_down $ nodes

let info_tear_down =
  let doc = "Stops the Tezos node and destroys the Deku state." in
  Cmd.info "tear-down" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let setup =
  let open Term in
  const setup $ mode $ nodes

let info_setup =
  let doc =
    "Does the following: it starts a Tezos sandbox network with Flextesa, then \
     it generates a new validator indentities and it deploys a new contract to \
     the Tezos sandbox configured to use these validators." in
  Cmd.info "setup" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

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

let deposit_withdraw_test =
  let open Term in
  const deposit_withdraw_test $ mode $ nodes

let info_deposit_withdraw_test =
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

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-sandbox"

let load_test =
  let open Term in
  const load_test $ const ()

let info_load_test =
  let doc = "Load tests a local running Deku cluster" in
  Cmd.info "load-test" ~version:"%\226\128\140%VERSION%%" ~doc ~exits

let check_liveness =
  let open Term in
  const check_liveness $ mode

let info_check_livenss =
  let doc =
    "Checks that the Deku cluster is producing blocks and posting to the main \
     chain" in
  Cmd.info "check-liveness" ~version:"%\226\128\140%VERSION%%" ~doc ~exits

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
         Cmd.v info_start start;
         Cmd.v info_setup setup;
         Cmd.v info_tear_down tear_down;
         Cmd.v info_deposit_withdraw_test deposit_withdraw_test;
         Cmd.v info_deploy_dummy_ticket deploy_dummy_ticket;
         Cmd.v info_deposit_dummy_ticket deposit_dummy_ticket;
         Cmd.v info_load_test load_test;
         Cmd.v info_check_livenss check_liveness;
       ]
