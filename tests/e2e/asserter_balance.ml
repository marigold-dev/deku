open Cmdliner
open Bin_common

let assert_state_correct data_folder contract ticket =
  print_endline @@ "Checking state at path: " ^ data_folder;
  let file = data_folder ^ "/state.bin" in
  let protocol_state = Lwt_main.run @@ Files.State_bin.read ~file in
  let state = protocol_state.core_state in
  let open Core_deku in
  let contract = Address.of_string contract |> Option.get in
  let ticket = Tezos.Ticket_id.of_string ticket |> Option.get in
  let ledger = State.ledger state in
  let balance = Ledger.balance contract ticket ledger |> Amount.to_int in
  assert (balance = 90);
  print_endline "State looks good 👍"

let args =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let addr =
    let docv = "addr" in
    let doc = "the expected contract" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let ticket =
    let docv = "ticket" in
    let doc = "The ticket" in
    let open Arg in
    required & pos 2 (some string) None & info [] ~doc ~docv in

  let open Term in
  const assert_state_correct $ folder_node $ addr $ ticket

let _ = Cmd.eval @@ Cmd.v (Cmd.info "asserter") args
