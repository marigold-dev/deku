open Protocol
open Helpers

type t = {
  blocks : Block.t list;
  block_height : int64;
}

let in_memory = ref { blocks = []; block_height = 0L }

let init () =
  (* TODO: Ensure the "database" folder exists *)

  (* find the file with the biggest height *)
  let%await database =
    Lwt_unix.files_of_directory "database" |> Lwt_stream.to_list in
  let last_file =
    database
    |> List.filter (fun file -> file <> ".." && file <> ".")
    |> List.sort (fun file1 file2 ->
           let height1 =
             String.split_on_char '-' file1
             |> List.rev
             |> List.hd
             |> Int64.of_string in
           let height2 =
             String.split_on_char '-' file2
             |> List.rev
             |> List.hd
             |> Int64.of_string in
           Int64.sub height2 height1 |> Int64.to_int)
    |> fun ls -> List.nth_opt ls 0 in

  match last_file with
  | None -> await ()
  | Some file ->
    (* we should load this file in memory *)
    let%await string =
      Lwt_io.with_file ~mode:Input ("database/" ^ file) Lwt_io.read in
    let json = Yojson.Safe.from_string string in
    let%await blocks =
      match [%of_yojson: Block.t list] json with
      | Ok data -> await data
      | Error _ -> failwith "Database is corrupted" in
    (* If I have 100 blocks, it means the file is full, so we do not load the blocks, only the the block_height *)
    let last_block = List.hd blocks in
    let block_height = last_block.block_height in
    if List.length blocks = 100 then
      in_memory := { blocks = []; block_height }
    else
      in_memory := { blocks; block_height };
    await ()

let add block =
  let state = !in_memory in
  let next_state =
    { blocks = block :: state.blocks; block_height = block.block_height } in
  (* Saved on disk every 100 blocks (so that the file to write to disk has a constant size) *)
  if List.length next_state.blocks = 100 then (
    let () = print_endline "save on disk" in
    let last = (List.hd next_state.blocks).block_height in
    let first = (List.rev next_state.blocks |> List.hd).block_height in
    let file_name =
      Format.sprintf "database/%s-blocks-%s" (first |> Int64.to_string)
        (last |> Int64.to_string) in
    (*The file doesn't exists*)
    let%await _ =
      Lwt_io.with_file ~mode:Output file_name (fun oc ->
          let json = [%to_yojson: Block.t list] next_state.blocks in
          Lwt_io.write oc (Yojson.Safe.pretty_to_string json)) in
    (* So my next state is*)
    in_memory := { blocks = []; block_height = next_state.block_height };
    await block)
  else (
    in_memory := next_state;
    await block)

let level () = !in_memory.block_height
