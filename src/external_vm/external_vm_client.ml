open External_vm_protocol

exception Vm_lifecycle_error of string
exception Vm_execution_error of string

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
         ~of_yojson:External_vm_protocol.vm_server_message_of_yojson)

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

let apply_vm_operation_exn ~state ~source ~tickets operation =
  match !vm with
  | Some vm ->
      (* TODO: this is a dumb way to do things. We should have a better protocol than JSON. *)
      vm.send
        (Transaction
           { source; operation; tickets; operation_hash_raw = operation });
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
        | Take_tickets _ | Deposit_tickets _ -> failwith "FIXME:"
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
