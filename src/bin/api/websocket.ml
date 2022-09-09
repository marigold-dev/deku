open Deku_consensus
open Deku_stdlib

type t = Dream.websocket
type message = New_block of Block.t

let yojson_of_message message =
  match message with
  | New_block block -> `Assoc [ ("NEW_BLOCK", Block.yojson_of_t block) ]

module Map = Map.Make (Uuidm)

(** Sends a message to a client *)
let send t message =
  let message = yojson_of_message message |> Yojson.Safe.to_string in
  Dream.send t message

(** Broadcast a message to everyone *)
let broadcast websockets message =
  let%await () =
    Map.to_seq websockets |> List.of_seq |> List.map snd
    |> Lwt_list.iter_p (fun websocket -> send websocket message)
  in
  Lwt.return_unit
