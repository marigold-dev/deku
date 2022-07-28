open Debugger
open Protocol

type t = unit

let doc = "prev|p: Goes to the state at height-1"

let parse string =
  match string = "prev" || string = "p" with
  | true -> Some ()
  | false -> None

let process state () =
  let goto = Int64.sub state.current.block_height 1L in
  Goto.process state goto
