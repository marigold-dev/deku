open Protocol
open Debugger

type t = unit

let doc = "next|n: Goes to the state at height+1"

let parse string =
  match string = "next" || string = "n" with
  | true -> Some ()
  | false -> None

let process state () =
  let goto = Int64.add state.current.block_height 1L in
  Goto.process state goto
