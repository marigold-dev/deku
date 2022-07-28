open Protocol
open Helpers
open Files
open Debugger

type t = string option

let doc =
  "save <strint option>: Save the state on the disk, you can provide a custom \
   name."

let parse string =
  match string = "save" with
  | true -> Some None
  | false ->
  match String.split_on_char ' ' string with
  | ["save"; name] -> Some (Some name)
  | _ -> None

let process state filename =
  let file =
    filename
    |> Option.value
         ~default:
           (Printf.sprintf "state-%s.bin"
              (Int64.to_string state.current.block_height)) in
  let%await () = Config_files.State_bin.write state.current ~file in
  await (state, Msg (Printf.sprintf "saved %s on disk" file))
