
type node = (int, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node

type contract = node

val parse : string -> contract

val compile_contract : contract -> Binaryen.Module.t

val compile_value : node -> bytes