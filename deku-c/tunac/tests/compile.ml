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

let save_module wasm_mod filename =
  let output = open_out_bin filename in
  let mod_, _ = Binaryen.Module.write wasm_mod None in
  output_bytes output mod_;
  close_out output

open Cmdliner

let compile_contract print debug optimize shared_memory output =
  let config = Tunac.{ debug; shared_memory; optimize } in
  let wasm_mod = Tunac.compile_contract ~config contract in
  if print then
    Binaryen.Module.print wasm_mod;
  save_module wasm_mod output

let compile_value () =
  let value = Tunac.compile_value contract in
  print_bytes value

let debug =
  Arg.(value & flag & info [ "debug" ])

let optimize =
  Arg.(value & flag & info [ "optimize" ])

let shared_memory =
  Arg.(value & flag & info [ "shared-memory" ])

let print =
  Arg.(value & flag & info [ "print" ])

let output =
  Arg.(required & opt (some string) None & info [ "o"; "output" ])

let contract_cmd =
  Cmd.v (Cmd.info "contract")
    Term.(
      const compile_contract
      $ print
      $ debug
      $ optimize
      $ shared_memory
      $ output)

let value_cmd =
  Cmd.v (Cmd.info "value") Term.(const compile_value $ const ())

let compile_cmd =
  Cmd.group (Cmd.info "compile") [ contract_cmd; value_cmd ]

let () =
  exit (Cmd.eval compile_cmd)