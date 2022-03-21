open Core

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks"
       [
         ("gas", Bench_gas.command);
         ("recursive", Bench_recursion.command);
         ("expr", Bench_simple_expr.command);
       ])

let () = main ()
