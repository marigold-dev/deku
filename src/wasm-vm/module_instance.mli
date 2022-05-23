open Wasm
type t = Instance.module_inst
val make :
  gas:int ref -> module_:Module.t -> custom:(Memory.t -> int64 -> unit) -> t
val get_memory : t -> (Memory.t, Errors.t) result
val get_entrypoint : t -> (Instance.func_inst, [> `Execution_error]) result
