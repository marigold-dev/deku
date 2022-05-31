open Core_bench
open Core

let bench_big_map_hash =
  Bench.Test.create ~name:"consensus: hash big map" (fun () ->
      let _ = Map_hash.hash_the_map ~bmap_size:200_000 ~smap_size:5 in
      ())

(* Test with alcotest first *)
let test_state_hash =
  let test_list, _state = Build_state.build_state () in
  ("Alcotest - State hash", [test_list] |> List.concat)

let bench_state_hash =
  let _ =
    let open Alcotest in
    run "Alcotest state hash" [test_state_hash] in
  Bench.Test.create ~name:"Bench state hash" (fun () ->
      let _test_list, state = Build_state.build_state () in
      let _ = Core_deku.State.hash state in
      ())

let tests = [bench_big_map_hash; bench_state_hash]

let command = Bench.make_command tests

(* Build
   $ nix develop -c dune build
   $ nix develope -c dune exec -- ./benchmarks/state_hash/state_hash.exe subcommand
    - state-hash: benchmark the state root hash

    or using esy:
   $ esy b dune build
   $ esy b dune exec ./benchmarks/state_hash/state_hash.exe subcommand
*)

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks" [("state-hash", command)])

let () = main ()
