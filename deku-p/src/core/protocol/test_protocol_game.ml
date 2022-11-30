open Deku_stdlib
open Deku_concepts
open Protocol

let level_of_int i = 
  Z.of_int i |> N.of_z |> Option.get |> Level.of_n

let%expect_test "attest my twitch handle" =
  let current_level = level_of_int 0 in
  let x = apply ~current_level  in
  Format.printf "hello";
  [%expect {| hello |}]
