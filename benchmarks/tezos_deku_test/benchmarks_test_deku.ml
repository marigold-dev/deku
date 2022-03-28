open Core

(* TODO: script for running benchmark

   $ dune build @benchmarks/bench_test_deku
   $ dune exec -- ./benchmarks/tezos_deku_test/benchmarks_test_deku.exe subcommand

    - rpc: for tezos_rpc
    - ledger: for src/core_deku/ledger.ml
    - validators: for src/protocol/validators.ml
    - patricia: for incremental patricia tree
    - interop: for tezos internal operations
    -

    or using esy:
   $ esy b dune build
   $ esy b dune exec ./benchmarks/tezos_deku_test/benchmarks_test_deku.exe subcommand
*)

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks"
       [("ledger", Bench_test_ledger.command)])

let () = main ()