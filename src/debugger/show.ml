open Protocol
open Helpers
open Debugger

type t = unit

let doc = "show: Prints the current state."

let parse string =
  match string = "show" with
  | true -> Some ()
  | false -> None

let process state () =
  let output =
    state.current
    |> Protocol_state.to_yojson
    |> Yojson.Safe.Util.to_assoc
    |> List.filter (fun (field, _) -> field <> "included_tezos_operations")
    |> (fun ls -> `Assoc ls)
    |> Yojson.Safe.pretty_to_string
    |> fun msg -> Msg msg in
  await (state, output)
