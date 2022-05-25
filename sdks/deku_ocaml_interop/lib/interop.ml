let fd_pair = ref None

let init_fifo () = 
  let path = Array.get Sys.argv 1 in
  let machine_to_chain_path = path ^ "_read" in
  let chain_to_machine_path = path ^ "_write" in
  let machine_to_chain = Unix.openfile machine_to_chain_path [Unix.O_WRONLY] 0o666 in
  let chain_to_machine = Unix.openfile chain_to_machine_path [Unix.O_RDONLY] 0o666 in
  fd_pair := Some (machine_to_chain, chain_to_machine);
  (machine_to_chain, chain_to_machine)

let machine_to_chain () = match !fd_pair with
| None -> failwith "should be opened at initialization"
| Some (machine_to_chain, _) -> machine_to_chain

let chain_to_machine () = match !fd_pair with
| None -> failwith "should be opened at initialization"
| Some (_, chain_to_machine) -> chain_to_machine

let read ~fd () : Bytes.t =
  let message_length  = Bytes.create 8 in
  let _ = Unix.read fd message_length 0 8 in
  let message_length = Bytes.get_int64_ne message_length 0 |> Int64.to_int in
  let message = Bytes.create message_length in
  let _ = Unix.read fd message 0 message_length in
  message

let write ~fd (message: Bytes.t): unit =
  let bytes_length = Bytes.length message in
  let message_length = Bytes.create 8  in
  Bytes.set_int64_ne message_length 0 (Int64.of_int bytes_length);
  let _ = Unix.write fd message_length 0 8 in
  let _ = Unix.write fd message 0 bytes_length in
  ()

let get key =
  let machine_to_chain = machine_to_chain () in
  let chain_to_machine = chain_to_machine () in
  let message = `List [`String "Get"; `String key]
    |> Yojson.Safe.to_string
    |> Bytes.of_string in 
  write ~fd:machine_to_chain message;
  read ~fd:chain_to_machine ()
    |> String.of_bytes
    |> Yojson.Safe.from_string

let set key value =
  let machine_to_chain = machine_to_chain () in
  let message = `List [`String "Set"; `Assoc ["key", `String key; "value", value]]
    |> Yojson.Safe.to_string
    |> Bytes.of_string in
  write ~fd:machine_to_chain message

let main _initial_state state_transition =
  print_endline "start the vm";
  let (machine_to_chain, chain_to_machine) = init_fifo () in
  print_endline "fifo opened";


  (* Uncomment when PR #628 is merged *)
  (* let init = read ~fd:chain_to_machine () |> Bytes.to_string in
  print_endline "receive init message";

  if init = "\"init\""
  then
    let payload = `Assoc initial_state 
      |> Yojson.Safe.to_string
      |> Bytes.of_string
    in
    write ~fd:machine_to_chain payload; *)


  print_endline "initialized";
  (* endless loop *)
  let rec runtime_loop () =
    let control = read ~fd:chain_to_machine () |> Bytes.to_string in
    if control = "\"close\"" 
      then ()
    else
      let sender = read ~fd:chain_to_machine () |> Bytes.to_string in (* TODO: convert to an address type *)
      let tx_hash = read ~fd:chain_to_machine () |> Bytes.to_string in (* TODO: convert to an hash type *)
      let input = read ~fd:chain_to_machine () in
      let error = state_transition sender tx_hash input in
      let payload = (match error with
        | Error reason -> `List [`String "Error"; `String reason]
        | Ok () -> `List [`String "Stop"])
      |> Yojson.Safe.to_string |> Bytes.of_string in
      write ~fd:machine_to_chain payload;
      runtime_loop () in
  
  runtime_loop ()