open Helpers
open Crypto
open External_vm_protocol

let vm = ref None

let start_vm_ipc ~named_pipe_path =
  vm :=
    Some
      (External_process.open_vm_pipes ~named_pipe_path
         ~to_yojson:vm_client_message_to_yojson
         ~of_yojson:External_vm_protocol.vm_server_message_of_yojson)

let get_initial_state () =
  match !vm with
  | None ->
    failwith
      "You must initialize the external VM IPC before getting the initial state"
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
      "You must initialize the external VM IPC before sending the initial state"
  | Some vm -> vm.send (Set_Initial_State state)

let apply_vm_operation ~state ~source ~tx_hash operation =
  match (state, !vm) with
  | Some state, Some vm ->
    (* TODO: I'm using the first message as a control, but we should have a dedicated control pipe.
       For now, I send an empty message if there's nothing extra to do. *)
    vm.send Control;
    (* TODO: this is a dumb way to do things. We should have a better protocol than JSON. *)
    vm.send (Source (Key_hash.to_string source));
    vm.send (Tx_hash (BLAKE2B.to_string tx_hash));
    vm.send (Operation operation);
    let finished = ref false in
    let state = ref state in
    while not !finished do
      match vm.receive () with
      | Init _ -> failwith "Init shouldn't be received, TODO: better error"
      | Stop -> finished := true
      | Set { key; value } -> state := State.set key value !state
      | Error message ->
        Format.eprintf "VM error: %s\n%!" message;
        finished := true
    done;
    !state
  | None, Some _vm ->
    failwith "You must intialize the Vm state before applying a Vm transaction"
  | _, None ->
    failwith "You must initialize the Vm IPC before applying a Vm transaction"

let close_vm_ipc () =
  match !vm with
  | Some { close; _ } ->
    close ();
    vm := None
  | None -> failwith "Cannot close the VM as it was not opened."
