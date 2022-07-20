open Cmdliner
open Sandbox_helpers.Cmdliner_helpers

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
