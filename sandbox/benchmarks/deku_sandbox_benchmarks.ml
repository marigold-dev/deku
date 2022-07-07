open Cmdliner
open Sandbox_helpers.Cmdliner_helpers

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-sandbox-benchmarks"

let default_info =
  let doc =
    "benchmarks: tps, tps off-line, network messages on Deku clusters in a \
     sandbox mode suitable for local development and testnets. BE ADVISED: \
     some of the configuration options used by deku-sandbox-benchmarks are \
     unsafe for production environments. Refer to the production deployment \
     guide." in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-sandbox-benchmarks" ~version:"%\226\128\140%VERSION%%" ~doc
    ~sdocs ~exits

let _ =
  Cli.make ~info:default_info ()
  |> Cli.add (module Load_test_tps)
  |> Cli.add (module Load_test_tps_offline)
  |> Cli.add (module Network_msg)
  |> Cli.eval
