open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

(* avoid pretty printing *)
let () = Unix.putenv "TERM" "dumb"

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_michelson_insertion_1.ligo" ] ;
  [%expect{xxx|
    File "../../test/contracts/negative/bad_michelson_insertion_1.ligo", line 4, characters 32-74:
      3 | function main (const p : nat; const s: nat ) : list (operation)* nat is block {
      4 |   const f : (nat * nat -> nat)= [%Michelson ({| ADD |} : nat *nat -> nat)];
      5 | } with ((nil: list(operation)), f (p, s))

    Raw Michelson must be seq (with curly braces {}), got: ADD. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_michelson_insertion_2.ligo" ] ;
  [%expect{xxx|
File "../../test/contracts/negative/bad_michelson_insertion_2.ligo", line 5, characters 32-40:
  4 |   const f : (nat -> nat -> nat)= [%Michelson ({| ADD |} : nat -> nat -> nat)];
  5 | } with ((nil: list(operation)), f (p, s))

Invalid type(s).
Expected: "nat", but got: "( nat * nat )". |xxx}]
