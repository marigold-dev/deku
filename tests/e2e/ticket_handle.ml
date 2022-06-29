open Cmdliner

let create_handle =
  let open Core_deku in
  let open Contracts in
  let arg = [%to_yojson: bytes] (Int64.zero |> Context.Ticket_handle.to_bytes) in
  Format.printf "%s\n%!" (Yojson.Safe.to_string arg)

let args =
  let open Term in
  const create_handle

let _ = Cmd.eval @@ Cmd.v (Cmd.info "ticket-handle") args
