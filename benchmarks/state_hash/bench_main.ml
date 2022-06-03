open Core_bench
open Core

let bench_state_hash =
  Bench.Test.create ~name:"bench state hash" (fun () ->
      let _ = Build_state.build_state () in
      ())

let tests = [bench_state_hash]

let command = Bench.make_command tests

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks" [("state-hash", command)])

let () = main ()
