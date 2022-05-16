open Cmdliner

let args =
  let open Arg in
  let ticketer =
    let doc = "Tezos address of the contract issusing the ticket (e.g KT1..." in
    required & pos 1 (some string) None & info [] ~doc ~docv:"ticketer" in
  let open Term in
  const Transfers_tests.load_test_transactions $ ticketer

let main = Term.exit @@ Term.eval (args, Term.info "bench-tps")
