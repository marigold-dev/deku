open Cmdliner
open External_vm
open Deku_vm

let map_error error =
  match error with
  | `Invocation_error error -> "Invocation_error: " ^ error
  | `Origination_error error -> "Origination_error: " ^ error

let transition storage sender _tx_hash operation_hash operation =
  let operation = Contract_transaction.of_yojson operation in
  match operation with
  | Ok operation ->
    Contract_transaction.transaction storage sender operation_hash operation
    |> Result.map_error map_error
  | Error _ -> Error "unknown operation"

let deku_vm named_pipe_path =
  External_vm_server.main ~named_pipe_path Contract_storage.initial_state
    transition

let node =
  let named_pipe =
    let docv = "named_pipe" in
    let doc =
      "Path to the named pipes used for IPC with the chain. Will suffix with \
       '_read' and '_write' respectively." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const deku_vm $ named_pipe

let _ = Cmd.eval @@ Cmd.v (Cmd.info "deku-vm") node
