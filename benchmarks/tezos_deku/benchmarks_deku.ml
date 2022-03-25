open Core

(* TODO: script for running benchmark
   dune build @benchmarks/bench
   dune exec -- ./benchmarks/tezos_deku/benchmarks_deku.exe subcommand
   - rpc:
   or using esy:
   esy b dune build
   esy b dune exec ./benchmarks/tezos_deku/benchmarks_deku.exe subcommand
*)

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks"
       [("rpc", Bench_tezos_rpc.command)])

let () = main ()