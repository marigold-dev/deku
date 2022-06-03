type t = Wasm.Instance.memory_inst
val load : t -> address:int64 -> int
val store_bytes : t -> address:int64 -> content:bytes -> unit
val load_bytes : t -> address:int64 -> size:int -> bytes
