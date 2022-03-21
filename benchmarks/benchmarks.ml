open Core

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks" [("basic", Basic.command)])

let () = main ()
