
type node = (int, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node

type contract = node

type config =
  { debug : bool
  ; shared_memory : bool
  ; optimize : bool
  ; memory : int * int  }

val parse : string -> contract

val compile_contract : config:config -> contract -> Binaryen.Module.t

val compile_value : node -> bytes