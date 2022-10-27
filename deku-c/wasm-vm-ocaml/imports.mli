open Wasm

exception Type_error

val imports : 'inst. unit -> (Utf8.t * Instance.extern) list
val alloc : Value.t * Value.t -> int64
val read : int64 -> Value.t
