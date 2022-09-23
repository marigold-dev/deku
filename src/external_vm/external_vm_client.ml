open External_vm_protocol

type error = [ `Vm_lifecycle_error of string | `Vm_execution_error of string ]

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

let rec while_result acc f =
  let open Deku_stdlib in
  let%ok cond, acc = f acc in
  match cond with true -> while_result acc f | false -> Ok acc

let apply_vm_operation ~state ~source ~tickets operation =
  let open Deku_stdlib in
  let%ok vm =
    Option.to_result
      ~none:
        (`Vm_lifecycle_error
          "You must initialize the Vm IPC before applying a Vm transaction")
      !vm
  in
  (* TODO: this is a dumb way to do things. We should have a better protocol than JSON. *)
  vm.send
    (Transaction { source; operation; tickets; operation_hash_raw = operation });
  while_result state (fun state ->
      match vm.receive () with
      | Init _ ->
          Error
            (`Vm_lifecycle_error
              "Init shouldn't be received, TODO: better error")
      | Stop -> Ok (false, state)
      | Set { key; value } -> Ok (true, State.set key value state)
      | Take_tickets _ | Deposit_tickets _ -> failwith "TODO"
      | Error message ->
          Error
            (`Vm_execution_error (Format.sprintf "VM error: %s\n%!" message)))

let close_vm_ipc () =
  match !vm with
  | Some { close; _ } ->
      close ();
      vm := None
  | None -> failwith "Cannot close the VM as it was not opened."
