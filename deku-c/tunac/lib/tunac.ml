
type node = (int, string) Tezos_micheline.Micheline.node

type contract = node

type config =
  { debug : bool
  ; shared_memory : bool
  ; optimize : bool
  ; memory : int * int }

let parse code =
  let open Tezos_micheline in
  let tokens, _ = Micheline_parser.tokenize code in
  let code, _ = Micheline_parser.parse_expression tokens in
  code
  |> Micheline.strip_locations
  |> Micheline.root

let _print_node fmt node =
  let open Tezos_micheline in
  node
  |> Micheline.strip_locations
  |> Micheline_printer.printable Michelson_v1_primitives.string_of_prim
  |> Micheline_printer.print_expr fmt

let report error =
  let open IR_of_michelson in
  let open Format in
  match error with
  | Invalid_contract_format ->
    print_endline "Invalid contract format"
  | Unsupported_instruction ->
    printf "Unsupported Michelson instruction: \n"
  | Unsupported_parameter_type ->
    printf "Unsupported parameter type: \n"
  | Unsupported_storage_type ->
    printf "Unsupported storage type: \n"

(* TODO: Return result instead of exit *)
let report_error = function
  Ok c -> c | Error err -> report err; exit 1

let compile_contract ~config:_ contract =
  let open Lwt_result.Syntax in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let canonical_contract = Result.get_ok @@ Protocol.Michelson_v1_primitives.prims_of_strings contract in
  let+ typed_contract, _ =
    let code = lazy_expr canonical_contract in
    Protocol.Script_ir_translator.parse_code
      (dummy_environment ()).tezos_context
      ~legacy:false
      ~code:code
  in
  let contract = report_error @@ IR_of_michelson.compile_contract typed_contract in

  let obj =
    let filename, output = Filename.open_temp_file ~mode:[] "contract" ".ll" in
    let fmt = Format.formatter_of_out_channel output in
    Llvm_of_ir.compile_ir fmt contract;
    close_out output;

    let objfile = Filename.temp_file "contract" ".wasm" in
    Llvm_of_ir.compile_llvm_to_wasm filename objfile;
    objfile
  in
  obj

let compile_contract ~config contract =
  let contract = Tezos_micheline.Micheline.strip_locations contract in
  Lwt_result.map_error (fun _ -> "Error") @@ compile_contract ~config contract

let compile_value = Serialize.compile_value

let link = Linking.link_contract