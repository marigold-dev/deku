open Cmdliner
open Bin_common

let assert_state_correct data_folder contract ticket amount =
  print_endline @@ "Checking state at path: " ^ data_folder;
  let file = data_folder ^ "/state.bin" in
  let protocol_state = Lwt_main.run @@ Files.State_bin.read ~file in
  let state = protocol_state.core_state in
  let open Core_deku in
  let contract = Address.of_string contract |> Option.get in
  let table = State.ledger state |> Ledger.ticket_table in
  let contract_storage = State.contract_storage state in
  let ticket = Tezos.Ticket_id.of_string ticket |> Option.get in
  assert (
    Contract_storage.get_contract
      ~address:(Address.to_contract_hash contract |> Option.get)
      contract_storage
    |> Option.is_some);
  let balance =
    Ticket_table.balance table ~sender:contract ~ticket
    |> Option.value ~default:Amount.zero in
  assert (Amount.(equal balance (of_int amount)));
  print_endline "State looks good 👍"

let args =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let contract_address =
    let docv = "contract_address" in
    let doc = "the expected contract" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let ticket =
    let docv = "ticket" in
    let doc = "The ticket" in
    let open Arg in
    required & pos 2 (some string) None & info [] ~doc ~docv in
  let amount =
    let docv = "amount" in
    let doc = "The ticket amount" in
    let open Arg in
    required & pos 3 (some int) None & info [] ~doc ~docv in
  let open Term in
  const assert_state_correct $ folder_node $ contract_address $ ticket $ amount

let () = Term.exit @@ Term.eval (args, Term.info "asserter")
