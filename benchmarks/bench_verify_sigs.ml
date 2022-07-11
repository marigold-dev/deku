open Core_bench
open Core

let make_test info =
  Bench.Test.create ~name:info (fun () ->
      let _ = () in
      ())

let tests = ()

let command = Bench.make_command tests

let main () =
  Command_unix.run
    (Command.group ~summary:"Benchmark" [("verify-sigs", command)])

let () = main ()
