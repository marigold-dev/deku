open Wasm

type t = Memory.t

let load t ~address = Memory.load_byte t address

let store_bytes t ~address ~content =
  Memory.store_bytes t address (Bytes.to_string content)

let load_bytes t ~address ~size =
  Bytes.of_string (Memory.load_bytes t address size)
