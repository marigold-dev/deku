open Deku_indexer
open Deku_stdlib
open Deku_tezos

type t = { indexer : Indexer.t; consensus : Address.t; discovery : Address.t }

let state : t option ref = ref None

let make ~database_uri ~consensus ~discovery =
  let config = Indexer.{ save_messages = true; save_blocks = true } in
  let%await indexer = Indexer.make ~uri:database_uri ~config in
  state := Some { indexer; discovery; consensus };
  Lwt.return ()

let get_state () =
  match !state with
  | Some state -> state
  | None -> failwith "inintialized state"
