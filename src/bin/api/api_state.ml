open Deku_indexer
open Deku_stdlib

type t = { indexer : Indexer.t }

let state : t option ref = ref None

let make ~database_uri =
  let%await indexer = Indexer.make ~uri:database_uri in
  state := Some { indexer };
  Lwt.return ()

let get_state () =
  match !state with
  | Some state -> state
  | None -> failwith "inintialized state"
