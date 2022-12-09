[@@@warning "-32-69-37"]

open Ocaml_wasm_vm

let read_file name =
  let f = open_in name in
  let buf = In_channel.input_all f in
  buf

let originate contract init =
  let tickets, init = Tunac.Compiler.compile_value init |> Result.get_ok in
  let inputs =
    if Core.String.is_suffix ~suffix:"tz" contract then read_file contract
    else contract
  in
  let wat, constants, entrypoints =
    inputs |> Tunac.Compiler.compile |> Result.get_ok
  in
  let out = Tunac.Output.make wat constants |> Result.get_ok in
  let entrypoints = entrypoints |> Option.value ~default:[] in
  Operation_payload.
    {
      tickets;
      operation =
        Operation.Originate
          {
            module_ = out.module_;
            entrypoints = Entrypoints.of_assoc entrypoints;
            constants;
            initial_storage = init;
          };
    }
  |> Data_encoding.Json.construct Operation_payload.encoding
  |> Data_encoding.Json.to_string |> print_endline

let invoke address arg =
  let tickets, init = Tunac.Compiler.compile_value arg |> Result.get_ok in

  Operation_payload.
    {
      tickets;
      operation =
        Operation.Call
          {
            address = Deku_ledger.Address.of_b58 address |> Option.get;
            argument = init;
          };
    }
  |> Data_encoding.Json.construct Operation_payload.encoding
  |> Data_encoding.Json.to_string |> print_endline

open Core

let originate =
  Command.basic
    ~summary:
      "Originate a smart contract with given [contract_code] and \
       [initial_storage]"
    ~readme:(fun () ->
      "Contract code = valid michelson contract.\n\
       Initial_storage = valid michelson value")
    Command.Let_syntax.(
      let%map_open base = anon ("contract_code" %: string)
      and storage = anon ("initial_storage" %: string) in
      fun () -> originate base storage)

let invoke =
  Command.basic
    ~readme:(fun () ->
      "Contract address = valid [DK1] address.\n\
       Contract argument = valid michelson value")
    ~summary:
      "Invoke a contract with given [contract_address] and [contract_argument]"
    (let%map_open.Command address = anon ("contract_address" %: string)
     and argument = anon ("contract_argument" %: string) in
     fun () -> invoke address argument)

let command =
  Command.group ~summary:"Originate/invoke contracts"
    [ ("originate", originate); ("invoke", invoke) ]

let () = Command_unix.run command
