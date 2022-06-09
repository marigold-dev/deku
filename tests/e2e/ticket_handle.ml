open Cmdliner

let create_handle addr ticket amount =
  let open Core_deku in
  let ticket = Tezos.Ticket_id.of_string ticket |> Option.get in
  let addr = Address.of_string addr |> Option.get in
  let amount = amount |> Amount.of_int in
  Format.printf "%s\n%!"
    (Ticket_handle.make addr ticket amount |> Ticket_handle.to_string)

let args =
  let contract_address =
    let docv = "address" in
    let doc = "the expected contract" in
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
  const create_handle $ contract_address $ ticket $ amount

let () = Term.exit @@ Term.eval (args, Term.info "ticket-handle")
