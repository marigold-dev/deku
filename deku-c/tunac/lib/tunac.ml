
type node = (int, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node

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
  |> Micheline.map (fun prim -> Michelson_v1_primitives.prim_of_string prim |> Result.get_ok)
  |> Micheline.root

let print_node fmt node =
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
  | Unsupported_instruction instr ->
    printf "Unsupported Michelson instruction: %a\n" print_node instr
  | Unsupported_parameter_type typ ->
    printf "Unsupported parameter type: %a\n" print_node typ
  | Unsupported_storage_type typ ->
    printf "Unsupported storage type: %a\n" print_node typ

(* TODO: Return result instead of exit *)
let report_error = function
  Ok c -> c | Error err -> report err; exit 1

let compile_contract ~config contract =
  let contract = report_error @@ IR_of_michelson.compile_contract contract in
  Wasm_of_ir.compile_ir
    ~memory:config.memory
    ~optimize:config.optimize
    ~debug:config.debug
    ~shared_memory:config.shared_memory
    contract

let compile_value = Serialize.compile_value