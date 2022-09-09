open Deku_indexer
open Deku_stdlib
open Deku_tezos

type t = {
  indexer : Indexer.t;
  consensus : Address.t;
  discovery : Address.t;
  websockets : Websocket.t Websocket.Map.t;
  random : Random.State.t;
  node : Uri.t;
}

let state : t option ref = ref None

let make ~database_uri ~consensus ~discovery ~node =
  let config = Indexer.{ save_messages = true; save_blocks = true } in
  let%await indexer = Indexer.make ~uri:database_uri ~config in
  let random = Random.State.make_self_init () in
  let websockets = Websocket.Map.empty in
  state := Some { indexer; discovery; consensus; websockets; random; node };
  Lwt.return ()

let get_state () =
  match !state with
  | Some state -> state
  | None -> failwith "inintialized state"

let register_websocket websocket t =
  let { indexer; consensus; discovery; websockets; random; node } =
    get_state ()
  in
  let uuid = Uuidm.v4_gen t.random () in
  let websockets = Websocket.Map.add uuid websocket websockets in
  let () =
    state := Some { websockets; consensus; discovery; indexer; random; node }
  in
  uuid

let remove_websocket uuid =
  let { indexer; consensus; discovery; websockets; random; node } =
    get_state ()
  in
  let websockets = Websocket.Map.remove uuid websockets in
  state := Some { indexer; consensus; discovery; websockets; random; node }
