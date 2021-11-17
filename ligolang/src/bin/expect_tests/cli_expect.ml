
(* exit <> 0 but expected exit = 0 *)
exception Should_exit_good

(* exit = 0 but expected exit <> 0 *)
exception Should_exit_bad

(* ugh, can we avoid this? *)
let () = Unix.putenv "TERM" "dumb"
let () = Unix.putenv "LIGO_FORCE_NEW_TYPER" "false"

let bad_test basename =
  "../../test/contracts/negative/" ^ basename

let test basename =
  "../../test/contracts/" ^ basename

(* Temporary breaking *)
let run_ligo args =
  Var.reset_counter ();
  let argv = Array.of_list ("ligo" :: args) in
  let result = Cli.run ~argv () in
  result

let run_ligo_good args =
  let exit_code = run_ligo args in
  if (exit_code <> 0)
  then raise Should_exit_good
  else ()

let run_ligo_bad args =
  let exit_code = run_ligo args in
  if (exit_code = 0)
  then raise Should_exit_bad
  else ()
