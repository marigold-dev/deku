[@@@warning "-32-69-37"]

let read_file name =
  let f = open_in name in
  let buf = Bytes.create 100000 in
  let size = input f buf 0 100000 in
  Bytes.to_string @@ Bytes.sub buf 0 size

type 'a t =
  { type_ : string
  ; tickets : (Tunac.Values.ticket_id * Tunac.Helpers.Z.t) list
  ; content : 'a
  }
[@@deriving yojson]

type originate_payload =
  { module_ : string
  ; constants : (int * Tunac.Values.t) array
  ; initial_storage : Tunac.Values.t
  ; entrypoints : Tunac.Path.t option
  }
[@@deriving yojson]

type invoke_payload =
  { address : string
  ; argument : Tunac.Values.t
  }
[@@deriving yojson]

let originate contract init =
  let tickets, init = Tunac.Compiler.compile_value init |> Result.get_ok in
  let inputs =
    if Core.String.is_suffix ~suffix:"tz" contract then read_file contract
    else contract
  in
  let wat, constants, entrypoints =
    inputs |> Tunac.Compiler.compile |> Result.get_ok
  in
  let out = Tunac.Output.make wat constants entrypoints |> Result.get_ok in
  { type_ = "Originate"
  ; tickets
  ; content =
      { module_ = out.Tunac.Output.module_
      ; constants = out.Tunac.Output.constants
      ; initial_storage = init
      ; entrypoints = out.Tunac.Output.entrypoints
      }
  }
  |> yojson_of_t yojson_of_originate_payload
  |> Yojson.Safe.pretty_to_string |> print_endline

let invoke address arg =
  let tickets, init = Tunac.Compiler.compile_value arg |> Result.get_ok in

  { type_ = "Invoke"; tickets; content = { address; argument = init } }
  |> yojson_of_t yojson_of_invoke_payload
  |> Yojson.Safe.pretty_to_string |> print_endline

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
