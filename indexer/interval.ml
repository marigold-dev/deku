open Helpers
open Network

(* Fetch the cluster every seconds *)
let rec run uri =
  let%await block_level_res = request_block_level () uri in
  let current_deku_level = block_level_res.level in
  let current_indexer_level = Repository.level () in

  let rec pull_blocks from_level to_level =
    if from_level = to_level then
      Lwt.return_unit
    else
      let () =
        print_endline
          (Printf.sprintf "fetching block: %s" (Int64.to_string from_level))
      in
      let%await block = request_block_by_level { level = from_level } uri in
      match block with
      | Some block ->
        let%await _ = Repository.add block in
        pull_blocks (Int64.add from_level 1L) to_level
      | None ->
        (*TODO: should throw error or do something*)
        let%await () = Lwt_unix.sleep 1.0 in
        pull_blocks from_level to_level in
  let%await () =
    pull_blocks (Int64.add current_indexer_level 1L) current_deku_level in
  let%await () = Lwt_unix.sleep 1.0 in
  (* Sleep between 2 request *) run uri

let is_sync uri =
  let%await block_level_res = request_block_level () uri in
  let current_deku_level = block_level_res.level in
  let current_indexer_level = Repository.level () in
  await (Int64.sub current_deku_level current_indexer_level < 5L)
(* There will always exists a delta between the indexer and the cluster*)
