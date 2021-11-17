Printexc.record_backtrace true ;
let () = Printf.printf "Start of mydebugplease executable\n%!" in
let test_result = Db_index_grouped_by_variable_tests.grouped_by_variable () (* Integration_tests.anon_function () *)(* Typer_tests.TestExpressions.lambda () *) in
let test (x : raise:'a Trace.raise -> unit) : unit = match Trace.to_stdlib_result x with
  | Stdlib.Ok () -> Format.printf "\ntest ok\n\n%!"
  | Stdlib.Error err -> Format.printf "\nMYDEBUGPLEASE ERROR:\n%a\n\n%!" (Main_errors.Formatter.error_ppformat ~display_format:Dev) err; failwith "ERROR" in
let () =
  test test_result in
let () = Printf.printf "End of mydebugplease executable\n%!" in
()
