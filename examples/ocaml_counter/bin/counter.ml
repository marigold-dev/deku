type action =
  | Increment
  | Decrement
[@@deriving yojson]

let process_action counter action =
  match action with
  | Increment -> counter + 1
  | Decrement -> counter - 1

let transition address hash input =
  print_endline (Printf.sprintf "from %s" address);
  print_endline (Printf.sprintf "hash %s" hash);
  print_endline (Printf.sprintf "input %s" (input |> Bytes.to_string));

  let action =
    action_of_yojson (Yojson.Safe.from_string (input |> Bytes.to_string)) in
  let counter = Interop.get "counter" |> [%of_yojson: int] in
  match (action, counter) with
  | Error _, _ -> Error "wrong json"
  | _, Error _ -> Error "wrong counter parsing"
  | Ok action, Ok counter ->
    print_endline "current counter value :";
    print_int counter;
    print_newline ();
    let next_counter = process_action counter action in
    Interop.set "counter" (`Int (next_counter + 1));
    Ok ()

let () = Interop.main [] transition
