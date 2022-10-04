open Deku_crypto
open Deku_stdlib
open External_vm_protocol

module Make (Ticket_id : sig
  type t [@@deriving yojson]
end) (Address : sig
  type t [@@deriving yojson]
end) =
struct
  exception Vm_lifecycle_error of string
  exception Vm_execution_error of string

  module External_vm_protocol_functor =
    External_vm_protocol.Make (Ticket_id) (Address)

  open External_vm_protocol_functor

  let () =
    Printexc.register_printer (function
      | Vm_lifecycle_error error ->
          Some (Format.sprintf "VM Lifecycle Error: %s" error)
      | Vm_execution_error error ->
          Some (Format.sprintf "VM Execution Error: %s" error)
      | _ -> None)

  let vm = ref None

  let start_vm_ipc ~named_pipe_path =
    vm :=
      Some
        (External_process.open_vm_pipes ~named_pipe_path
           ~to_yojson:yojson_of_vm_client_message
           ~of_yojson:External_vm_protocol_functor.vm_server_message_of_yojson)

  let get_initial_state () =
    match !vm with
    | None ->
        failwith
          "You must initialize the external VM IPC before getting the initial \
           state"
    | Some vm -> (
        (* TODO: we should some explicit definition of our protocol somewhere. *)
        vm.send Get_Initial_State;
        match vm.receive () with
        | Init set_list ->
            List.fold_left
              (fun state External_vm_protocol.{ key; value } ->
                State.set key value state)
              State.empty set_list
        | _ -> failwith "received a different message from Init")

  let set_initial_state state =
    match !vm with
    | None ->
        failwith
          "You must initialize the external VM IPC before sending the initial \
           state"
    | Some vm -> vm.send (Set_Initial_State state)

  type ledger_api =
    < take_tickets : Address.t -> (Ticket_id.t * N.t) list
    ; deposit : Address.t -> Ticket_id.t * N.t -> unit >

  let apply_vm_operation_exn ~state ~ledger_api ~source ~tickets ~level
      operation =
    match !vm with
    | Some vm ->
        (* TODO: this is a dumb way to do things. We should have a better protocol than JSON. *)
        (match operation with
        | None -> vm.send Noop_transaction
        | Some (operation_hash, operation) ->
            let operation_raw_hash =
              BLAKE2b.BLAKE2b_256.to_hex operation_hash
            in
            vm.send
              (Transaction
                 { source; operation; tickets; operation_raw_hash; level }));

        let finished = ref false in
        let state = ref state in
        while not !finished do
          match vm.receive () with
          | Init _ ->
              raise
                (Vm_lifecycle_error
                   "External VM protocol violated: received Init after already \
                    initialized")
          | Stop -> finished := true
          | Set { key; value } -> state := State.set key value !state
          | Deposit_tickets { tickets; address } ->
              List.iter
                (fun (ticket, amount) ->
                  ledger_api#deposit address (ticket, amount))
                tickets
          | Take_tickets address ->
              let tickets = ledger_api#take_tickets address in
              vm.send (Give_Tickets tickets)
          | Error message -> raise (Vm_execution_error message)
        done;
        !state
    | None ->
        raise
          (Vm_lifecycle_error
             "You must initialize the Vm IPC before applying a Vm transaction")

  let close_vm_ipc () =
    match !vm with
    | Some { close; _ } ->
        close ();
        vm := None
    | None -> failwith "Cannot close the VM as it was not opened."
end
