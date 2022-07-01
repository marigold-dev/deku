open Cmdliner

let create_handle =
  let open Smart_contracts in
  let arg = [%to_yojson: bytes] (Int32.zero |> Ticket_handle.to_bytes) in
  Format.printf "%s\n%!" (Yojson.Safe.to_string arg)

let args =
  let open Term in
  const create_handle

let _ = Cmd.eval @@ Cmd.v (Cmd.info "ticket-handle") args
