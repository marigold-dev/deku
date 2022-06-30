open Helpers
open Cmdliner
open Sandbox_flows
open Sandbox_helpers
open Cmdliner_helpers

let tear_down =
  let open Term in
  const tear_down $ nodes

let info_tear_down =
  let doc = "Stops the Tezos node and destroys the Deku state." in
  Cmd.info "tear-down" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* setup *)
let setup mode validators rpc_url =
  (* FIXME: this relative path seems suspicious - does it work if you move directories? *)
  let consensus = "./src/tezos_interop/consensus.mligo" in
  let discovery = "./src/tezos_interop/discovery.mligo" in
  let secret = "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq" in
  validators
  |> List.map (fun i -> Format.sprintf "data/%i" i)
  |> List.iter rm_dir;

  (* setup tezos-client *)
  let%assert () =
    ("the tezos node is not bootstrapped", is_node_bootstrapped rpc_url) in
  let%ok _ = tezos_client_update_config rpc_url in
  let%ok _ =
    import_secret rpc_url "myWallet" (Format.sprintf "unencrypted:%s" secret)
  in

  (* setup write indentity.json to file system *)
  let%ok identities = List.map_ok (Setup.setup_identity mode) validators in

  (* deploy smart contracts *)
  let consensus_storage = Setup.make_consensus_storage identities in
  let discovery_storage = Setup.make_discovery_storage identities in
  let%ok consensus_address =
    deploy_contract rpc_url "consensus" consensus consensus_storage "myWallet"
  in
  let%ok discovery_address =
    deploy_contract rpc_url "discovery" discovery discovery_storage "myWallet"
  in

  (* setup tezos informations *)
  Setup.make_trusted_validator_membership_change_json identities;
  identities
  |> List.map_ok
       (Setup.setup_tezos rpc_url secret consensus_address discovery_address)

let setup mode nodes =
  let validators = make_validators nodes in
  let rpc_url = rpc_url mode in
  setup mode validators rpc_url

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
         Cmd.v Start.info Start.term;
         Cmd.v Setup.info Setup.term;
         Cmd.v info_tear_down tear_down;
         Cmd.v info_deposit_withdraw_test deposit_withdraw_test;
         Cmd.v info_deploy_dummy_ticket deploy_dummy_ticket;
         Cmd.v info_deposit_dummy_ticket deposit_dummy_ticket;
         Cmd.v info_load_test load_test;
         Cmd.v info_check_livenss check_liveness;
       ]
