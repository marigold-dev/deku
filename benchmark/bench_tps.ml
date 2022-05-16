open Cmdliner



let args =
  let open Arg in
  let docv = "ticketer" in
  let ticketer =
    let doc = "Tezos address of the contract issusing the ticket (e.g KT1AC8Zapx3WwsahvLTapnn3tw5mSCaeKYSs)" in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const Transfers_tests.load_test_transactions $ ticketer

let main = Term.exit @@ Term.eval (args, Term.info "deku-bench-tps")
