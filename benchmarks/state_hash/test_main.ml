(*******************************************************************************)
(*
    nix develop -c dune build
    nix develop -c dune exec -- ./benchmarks/state_hash/test_main.exe
*)

(*let _test_state_hash_basic =
  let test_list, _state = Build_state.build_state () in
  ("State hash - Basic", [test_list] |> List.concat)
*)
let test_state_hash_3 =
  let test_list, _state = Build_state.build_state_3 () in
  ("State hash - 3", [test_list] |> List.concat)

  (*
let _test_state_hash_10 =
  let test_list, _state = Build_state.build_state_10 () in
  ("State hash - 10", [test_list] |> List.concat)*)

let main_alco () =
  let open Alcotest in
  run "Alcotest state hash"
    [test_state_hash_3]

let () = main_alco ()
