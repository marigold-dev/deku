(*******************************************************************************)
(* 
   nix develop -c dune build
   nix develop -c dune exec -- ./benchmarks/state_hash/test_main.exe 
*)

let test_state_hash_basic =
  let test_list, _state = Build_state.build_state () in
  ("State hash - Basic", [test_list] |> List.concat)

let test_state_hash_n =
  let test_list, _state = Build_state.build_state' () in
  ("State hash - n", [test_list] |> List.concat)

let main_alco () =
  let open Alcotest in
  run "Alcotest state hash" [test_state_hash_basic; test_state_hash_n]

let () = main_alco ()
