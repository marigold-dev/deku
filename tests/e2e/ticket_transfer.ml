open Cmdliner

let create_arg addr ticket amount =
  let open Core_deku in
  let ticket = Tezos.Ticket_id.of_string ticket |> Option.get in
  let addr = Address.of_string addr |> Option.get in
  let arg =
    Ticket_handle.make addr ticket (Amount.of_int amount)
    |> Ticket_handle.to_string in
  let args = arg ^ Address.to_string addr in
  Format.printf "%s\n%!" args

let args =
  let contract_address =
    let docv = "address" in
    let doc = " address of the contract owner" in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let ticket =
    let docv = "ticket" in
    let doc = "The ticket" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let amount =
    let docv = "amount" in
    let doc = "The ticket amount" in
    let open Arg in
    required & pos 2 (some int) None & info [] ~doc ~docv in
  let open Term in
  const create_arg $ contract_address $ ticket $ amount

let () = Term.exit @@ Term.eval (args, Term.info "ticket-transfer")
