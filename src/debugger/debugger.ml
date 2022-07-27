open Protocol
open Helpers

type t = {
  state : state;
  actions : (string -> state -> (state * output) option Lwt.t) list;
  docs : string list;
}

and state = {
  blocks : Block.t list;
  history : Protocol_state.t list;
  current : Protocol_state.t;
}

and output =
  | Msg of string
  | Nil

module type DEBUGGER_CMD = sig
  type t

  val doc : string

  val parse : string -> t option

  val process : state -> t -> (state * output) Lwt.t
end

let make blocks protocol_state history =
  {
    state = { blocks; history; current = protocol_state };
    actions = [];
    docs = [];
  }

let use (module Cmd : DEBUGGER_CMD) t =
  let run string t =
    let action = string |> Cmd.parse in
    match action with
    | None -> await None
    | Some action ->
      let%await result = Cmd.process t action in
      Some result |> await in
  let docs = List.append t.docs [Cmd.doc] in
  { t with actions = run :: t.actions; docs }

let process_output output =
  match output with
  | Nil -> print_newline ()
  | Msg msg -> print_endline msg

let show_help docs =
  "Deku Debugger Help:" :: docs |> String.concat "\n - " |> print_endline;
  print_newline ()

let prompt () =
  Printf.printf "deku> ";
  try read_line () with
  | End_of_file -> "quit"

let rec run t =
  let { state; actions; docs } = t in
  let input = prompt () in
  match input with
  | "quit" -> exit 0
  | "help" ->
    show_help docs;
    run t
  | input ->
    let%await results =
      actions |> Lwt_list.map_p (fun process -> process input state) in
    let next_state, output =
      results |> List.filter_map (fun res -> res) |> fun ls ->
      List.nth_opt ls 0
      |> Option.value
           ~default:(state, Msg "Unknown command, use help to see commands")
    in
    process_output output;
    run { t with state = next_state }
