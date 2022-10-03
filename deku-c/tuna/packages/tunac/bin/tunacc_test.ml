let read_file name =
  let f = open_in name in
  let buf = Bytes.create 100000 in
  let size = input f buf 0 100000 in
  Bytes.to_string @@ Bytes.sub buf 0 size

let compile_contract filename =
  let wat, constants, entrypoints =
    filename |> read_file |> Tunac.Compiler.compile |> Result.get_ok
  in
  let out = Tunac.Output.make wat constants entrypoints |> Result.get_ok in

  print_endline @@ Yojson.Safe.pretty_to_string @@ Tunac.Output.yojson_of_t out

let compile_value code =
  let value = code |> Tunac.Compiler.compile_value |> Result.get_ok in
  let out = value |> Tunac.Values.yojson_of_t |> Yojson.Safe.pretty_to_string in
  print_endline out

let () =
  match Sys.argv.(1) with
  | "contract" -> compile_contract Sys.argv.(2)
  | "value" -> compile_value Sys.argv.(2)
  | _ -> failwith "Invalid command"
