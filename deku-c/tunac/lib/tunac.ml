
type node = (int, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node

type contract = node

type config =
  { debug : bool
  ; shared_memory : bool
  ; optimize : bool }

let parse code =
  let open Tezos_micheline in
  let tokens, _ = Micheline_parser.tokenize code in
  let code, _ = Micheline_parser.parse_expression tokens in
  code
  |> Micheline.strip_locations
  |> Micheline.map (fun prim -> Michelson_v1_primitives.prim_of_string prim |> Result.get_ok)
  |> Micheline.root

let compile_contract ~config contract =
  let ir, env = IR_of_michelson.compile_contract contract in
  Wasm_of_ir.compile_ir
    ~optimize:config.optimize
    ~debug:config.debug
    ~shared_memory:config.shared_memory
    ~env
    ir

let compile_value = Serialize.compile_value