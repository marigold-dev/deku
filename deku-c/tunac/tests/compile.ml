let read_all () =
  (* FIXME: Doing concat directly results in a segfault ??? *)
  let rec aux s =
    try aux (input_line stdin :: s)
    with End_of_file -> s
  in
  aux []
  |> List.rev
  |> String.concat "\n"

let contract =
  let code = read_all () in
  Tunac.parse code

let save_module wasm_mod =
  let output = open_out_bin "mod.wasm" in
  let mod_, _ = Binaryen.Module.write wasm_mod None in
  output_bytes output mod_;
  close_out output

let _ =
  match Sys.argv.(1) with
  | "contract" ->
    let wasm_mod = Tunac.compile_contract contract in
    Binaryen.Module.print wasm_mod;
    save_module wasm_mod
  | "value" ->
    let value = Tunac.compile_value contract in
    print_bytes value
  | _ -> assert false
