open Core

(* TODO: script for running benchmark
   dune build @benchmarks/bench
   dune exec -- ./benchmarks/benchmarks.exe gas/recursive/expr/etc.
   or using esy:
   esy b dune build
   esy b dune exec ./benchmarks/benchmarks.exe gas
*)

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks"
       [
         ("gas", Bench_gas.command);
         ("recursive", Bench_recursion.command);
         ("expr", Bench_simple_expr.command);
         ("prim", Bench_prim.command);
       ])

let () = main ()
