open Helpers
open Protocol
open Debugger

type t = int64

let doc = "goto: Goes to the nth height of the chain."

let parse string =
  let input = String.split_on_char ' ' string in
  match input with
  | ["goto"; n] -> n |> Int64.of_string_opt
  | _ -> None

let process t height =
  let current =
    t.history
    |> List.find_opt (fun state -> Int64.equal state.block_height height) in
  match current with
  | None -> await (t, Msg "height out of reach")
  | Some current -> await ({ t with current }, Nil)
