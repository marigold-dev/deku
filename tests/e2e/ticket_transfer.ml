open Cmdliner

let create_arg addr =
  let open Core_deku in
  let open Contracts in
  let addr = Address.of_string addr |> Option.get in
  let arg = Int32.zero |> Context.Ticket_handle.to_bytes in
  let args =
    [%to_yojson: bytes]
      (Bytes.concat Bytes.empty
         [arg; Address.to_string addr |> String.to_bytes]) in
  Format.printf "%s\n%!" (Yojson.Safe.to_string args)

let args =
  let contract_address =
    let docv = "address" in
    let doc = " address of the contract owner" in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const create_arg $ contract_address

let _ = Cmd.eval @@ Cmd.v (Cmd.info "ticket-transfer") args
