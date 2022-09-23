open Deku_external_vm
open Deku_external_vm.External_vm_server

let transition storage source tickets operation =
  let _ = source in
  let _ = tickets in
  let _ = operation in
  let { set; get } = storage in
  let counter = get "counter" in
  print_endline operation;
  match counter with
  | None -> Error "counter not found"
  | Some counter ->
      let counter = int_of_string counter in
      print_endline @@ Format.sprintf "Counter: %i" counter;
      let counter =
        match operation with
        | "increment" -> counter + 1
        | "decrement" -> counter - 1
        | _ -> counter
      in
      set "counter" (counter |> string_of_int);
      Ok ()

let named_pipe_path = Array.get Sys.argv 1
let initial = External_vm_protocol.{ key = "counter"; value = "1" } :: []

let () =
  print_endline named_pipe_path;
  main ~named_pipe_path initial transition
