open Cmdliner
open Feather

let exits =
  Cmd.Exit.defaults
  @ [Cmd.Exit.info 1 ~doc:"expected failure (might not be a bug)"]

let man = [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

let run_ret cmd =
  match collect status cmd with
  | 0 -> `Ok 0
  | status ->
    `Error
      (false, Format.sprintf "Exited with status %d. See output above." status)

(* start *)
let start () = process "./sandbox.sh" ["start"] |> run_ret

let start =
  let open Term in
  const start $ const () |> ret

let info_start =
  let doc = "Starts a Deku cluster configured with this script" in
  Cmd.info "start" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* tear-down *)
let tear_down () = process "./sandbox.sh" ["tear-down"] |> run_ret

let tear_down =
  let open Term in
  const tear_down $ const () |> ret

let info_tear_down =
  let doc = "Stops the Tezos node and destroys the Deku state." in
  Cmd.info "tear-down" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* setup *)
let setup () = process "./sandbox.sh" ["setup"] |> run_ret

let setup =
  let open Term in
  const setup $ const () |> ret

let info_setup =
  let doc =
    "Does the following: it starts a Tezos sandbox network with Flextesa, then \
     it generates a new validator indentities and it deploys a new contract to \
     the Tezos sandbox configured to use these validators." in
  Cmd.info "setup" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* smoke test *)
let smoke_test () = process "./sandbox.sh" ["smoke-test"] |> run_ret

let smoke_test =
  let open Term in
  const smoke_test $ const () |> ret

let info_smoke_test =
  let doc =
    "Starts a Deku cluster and performs some simple checks that its working."
  in
  Cmd.info "smoke-test" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* deposit-withdraw-test *)
let deposit_withdraw_test () =
  process "./sandbox.sh" ["deposit-withdraw-test"] |> run_ret

let deposit_withdraw_test =
  let open Term in
  const deposit_withdraw_test $ const () |> ret

let info_deposit_withdraw_test =
  let doc =
    "Start a Deku cluster and originate a dummy tickets and performs a deposit \
     and a withdraw." in
  Cmd.info "deposit-withdraw-test" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man

(* deploy-dummy-ticket *)
let deploy_dummy_ticket () =
  process "./sandbox.sh" ["deploy-dummy-ticket"] |> run_ret

let deploy_dummy_ticket =
  let open Term in
  const deploy_dummy_ticket $ const () |> ret

let info_deploy_dummy_ticket =
  let doc =
    "Deploys a contract that forges dummy tickets and deposits to Deku." in
  Cmd.info "deploy-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

(* deposit-dummy-ticket *)
let deposit_dummy_ticket () =
  process "./sandbox.sh" ["deposit-dummy-ticket"] |> run_ret

let deposit_dummy_ticket =
  let open Term in
  const deposit_dummy_ticket $ const () |> ret

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
  @@ Cmd.eval'
  @@ Cmd.group default_info
       [
         Cmd.v info_start start;
         Cmd.v info_setup setup;
         Cmd.v info_smoke_test smoke_test;
         Cmd.v info_tear_down tear_down;
         Cmd.v info_deposit_withdraw_test deposit_withdraw_test;
         Cmd.v info_deploy_dummy_ticket deploy_dummy_ticket;
         Cmd.v info_deposit_dummy_ticket deposit_dummy_ticket;
       ]
