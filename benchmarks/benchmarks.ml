open Core

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks" [("gas", Bench_gas.command)])

let () = main ()
