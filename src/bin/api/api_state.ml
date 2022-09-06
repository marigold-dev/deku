open Deku_indexer
open Deku_stdlib

type t = { indexer : Indexer.t }

let state : t option ref = ref None

let make ~database_uri =
  let config = Indexer.{ save_blocks = true; save_messages = true } in
  let%await indexer = Indexer.make ~uri:database_uri ~config in
  state := Some { indexer };
  Lwt.return ()

let get_state () =
  match !state with
  | Some state -> state
  | None -> failwith "inintialized state"
