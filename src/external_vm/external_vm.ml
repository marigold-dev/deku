open Helpers
open Crypto

type t = Yojson.Safe.t String_map.t [@@deriving yojson]
let empty = String_map.empty |> String_map.add "counter" (`Int 0)
type vm_message =
  | Stop
  | Set   of {
      key : string;
      value : Yojson.Safe.t;
    }
  | Get   of string
  | Error of string
[@@deriving yojson]

let vm = ref None

let start_vm_ipc ~named_pipe_path =
  vm :=
    Some
      (External_process.open_pipes ~named_pipe_path ~to_yojson:Fun.id
         ~of_yojson:vm_message_of_yojson)

let apply_vm_operation ~state ~source ~tx_hash operation =
  match !vm with
  | Some vm ->
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
      | Stop -> finished := true
      | Set { key; value } -> state := String_map.add key value !state
      | Get key ->
        let value =
          String_map.find_opt key !state |> Option.value ~default:`Null in
        vm.send value
      | Error message ->
        Format.eprintf "VM error: %s\n%!" message;
        finished := true
    done;
    !state
  | None -> failwith "TODO: better error"

let close_vm_ipc () =
  match !vm with
  | Some { close; _ } ->
    close ();
    vm := None
  | None -> failwith "Cannot close the VM as it was not opened."
