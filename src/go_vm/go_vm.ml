open Helpers

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

let start_vm ~path ~named_pipe_path =
  vm :=
    Some
      (Go_process.spawn ~path ~named_pipe_path ~to_yojson:Fun.id
         ~of_yojson:vm_message_of_yojson)

let apply_vm_operation t operation =
  match !vm with
  | Some vm ->
    vm.send operation;
    let finished = ref false in
    let state = ref t in
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
