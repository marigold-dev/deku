open Wasm

type t = Memory.t

type address = int64

let blit t address content =
  Memory.store_bytes t address (Bytes.to_string content)

let sub t address size = Bytes.of_string (Memory.load_bytes t address size)
