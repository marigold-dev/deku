open Core_bench
open Core

(* state hash with base case:
   - 2 deposits
   - 4 basic user operation: contract origination;
    contract invocation; transfer and withdraw (2 accounts)
*)

let bench_state_hash_basic =
  Bench.Test.create ~name:"bench basic" (fun () ->
      let _test, state = Build_state.build_state () in
      let _ = Core_deku.State.hash state in
      ())

(* Increase case 
   
*)
let bench_state_hash_3 =
  Bench.Test.create ~name:"bench 3" (fun () ->
      let _test, state = Build_state.build_state_3 () in
      let _ = Core_deku.State.hash state in
      ())

let bench_state_hash_10 =
  Bench.Test.create ~name:"bench 10" (fun () ->
      let _test, state = Build_state.build_state_10 () in
      let _ = Core_deku.State.hash state in
      ())

let tests = [bench_state_hash_basic; bench_state_hash_3; bench_state_hash_10]

let command = Bench.make_command tests

(* Build
   $ nix develop -c dune build
   $ nix develope -c dune exec -- ./benchmarks/state_hash/bench_state_hash_main.exe subcommand
    - state-hash: benchmark the state root hash
*)

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks" [("state-hash", command)])

let () = main ()
