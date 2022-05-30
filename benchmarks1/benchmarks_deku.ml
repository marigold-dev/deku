open Core

(* TODO: script for running benchmark
   $ nix develop -c dune build
   $ nix develope -c dune exec -- ./benchmarks/tezos_deku/benchmarks_deku.exe subcommand
    - state-hash: benchmark the state root hash

    or using esy:
   $ esy b dune build
   $ esy b dune exec ./benchmarks/tezos_deku/benchmarks_deku.exe subcommand
*)

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks"
       [("state-hash", State_hash.command)])

let () = main ()
