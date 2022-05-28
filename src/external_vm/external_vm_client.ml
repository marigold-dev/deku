open Helpers
open Crypto
open External_vm_protocol

let vm = ref None

let start_vm_ipc ~named_pipe_path =
  vm :=
    Some
      (External_process.open_vm_pipes ~named_pipe_path ~to_yojson:Fun.id
         ~of_yojson:External_vm_protocol.vm_message_of_yojson)

let initial_state () =
  match !vm with
  | None ->
    failwith
      "You must initialize the external VM IPC before getting the initial state"
  | Some vm -> (
    (* TODO: we should some explicit definition of our protocol somewhere. *)
    vm.send (`String "init");
    match vm.receive () with
    | Init set_list ->
      List.fold_left
        (fun state External_vm_protocol.{ key; value } ->
          State.set key value state)
        State.empty set_list
    | _ -> failwith "received a different message from Init")

let apply_vm_operation ~state ~source ~tx_hash operation =
  match (state, !vm) with
  | Some state, Some vm ->
    (* TODO: I'm using the first message as a control, but we should have a dedicated control pipe.
       For now, I send an empty message if there's nothing extra to do. *)
    vm.send (`String "");
    (* TODO: this is a dumb way to do things. We should have a better protocol than JSON. *)
    let source = `String (Key_hash.to_string source) in
    vm.send source;
    vm.send (`String (BLAKE2B.to_string tx_hash));
    vm.send operation;
    let finished = ref false in
    let state = ref state in
    while not !finished do
      match vm.receive () with
      | Init _ -> failwith "Init shouldn't be received, TODO: better error"
      | Stop -> finished := true
      | Set { key; value } -> state := State.set key value !state
      | Get key ->
        let value = State.get key !state |> Option.value ~default:`Null in
        vm.send value
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
