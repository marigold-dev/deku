open Helpers
open Network

(* Fetch the cluster every seconds *)
let rec run_interval uri =
  let%await block_level_res = request_block_level () uri in
  let current_deku_level = block_level_res.level in
  let%await current_indexer_level = Repository.level () in

  let rec pull_blocks from_level to_level =
    if from_level = to_level then
      Lwt.return_unit
    else
      let () =
        print_endline
          (Printf.sprintf "fetching block: %s" (Int64.to_string from_level))
      in
      let%await response = request_block_by_level { level = from_level } uri in
      match response with
      | Some { block; timestamp = _ } ->
        let%await _ = Repository.add block in
        pull_blocks (Int64.add from_level 1L) to_level
      | None ->
        (* This case means the block is no more available from the node *)
        let%await () = Lwt_unix.sleep 1.0 in
        pull_blocks (Int64.add from_level 1L) to_level in
  let%await () =
    pull_blocks (Int64.add current_indexer_level 1L) current_deku_level in
  let%await () = Lwt_unix.sleep 1.0 in
  (* Sleep between 2 request *) run_interval uri

let rec run uri =
  let%await result =
    Lwt.catch
      (fun () ->
        let%await res = run_interval uri in
        Ok res |> await)
      (fun _ -> Error "The node is down" |> await) in
  match result with
  | Ok _ -> await ()
  | Error err ->
    print_endline err;
    (* If we can't request the node, we wait 5 seconds before re-request it*)
    let%await () = Lwt_unix.sleep 5.0 in
    run uri

let is_sync uri =
  let%await block_level_res = request_block_level () uri in
  let current_deku_level = block_level_res.level in
  let%await current_indexer_level = Repository.level () in
  await (Int64.sub current_deku_level current_indexer_level < 5L)
(* There will always exists a delta between the indexer and the cluster*)
