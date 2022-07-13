open Helpers
open Sandbox_flows
open Cmdliner
open Sandbox_helpers

let test_counter () =
  let get_counter () =
    Format.eprintf "Fetching counter rpc...\n%!";
    let%ok counter_str =
      curl ["-d"; "null"; "http://localhost:4440/counter"] |> run_res in
    Ok (int_of_string counter_str) in

  let%ok () = create_wallet deku_address deku_secret "wallet.json" in
  (* TODO: don't hard-code this *)
  let%ok counter_state = get_counter () in
  Format.printf "Submitting increment transaction \n%!";
  let%ok _ =
    deku_cli ["create-counter-transaction"; "./data/0"; "./wallet.json"]
    |> run_res in
  Format.printf "Transaction submitted\n%!";
  retry (fun () ->
      let%ok new_counter_state = get_counter () in
      let expected_counter_state = counter_state + 1 in
      if new_counter_state = expected_counter_state then (
        Format.printf "works 👌\n%!";
        Ok ())
      else
        Error "waiting for rpc to report counter update")

let term =
  let open Term in
  const test_counter $ const ()

let info =
  let doc = "Tests the deposit/withdraw scenario" in
  Cmd.info "counter-e2e-test" ~version:"%\226\128\140%VERSION%%" ~doc
