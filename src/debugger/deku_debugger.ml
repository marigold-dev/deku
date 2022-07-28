open Helpers
open Files
open Protocol
open Cmdliner

let load_state_bin state_bin = Config_files.State_bin.read ~file:state_bin

let load_blocks blocks =
  let%await blocks = Lwt_io.with_file ~mode:Input blocks Lwt_io.read in
  blocks
  |> Yojson.Safe.from_string
  |> [%of_yojson: Block.t list]
  |> Result.to_option
  |> Option.value ~default:[]
  |> List.sort (fun (b1 : Block.t) (b2 : Block.t) ->
         Int64.compare b1.block_height b2.block_height)
  |> await

let apply_block state block =
  Protocol.apply_block state block
  |> Result.map_error Node.Flows.string_of_error
  |> Result.map (fun (next, _, _) -> next)

let compute_history protocol_state blocks =
  blocks
  |> List.filter (fun (block : Block.t) ->
         Int64.compare block.block_height protocol_state.block_height > 0)
  |> List.fold_left
       (fun current_state block ->
         match current_state with
         | Error error -> Error error
         | Ok [] -> Error "can't be empty"
         | Ok (current_state :: states) ->
         match apply_block current_state block with
         | Ok next_state -> Ok (next_state :: current_state :: states)
         | Error error -> Error error)
       (Ok [protocol_state])
  |> Result.get_ok

let debugger state_bin blocks =
  let%await protocol_state = load_state_bin state_bin in
  let%await blocks = load_blocks blocks in
  let history = compute_history protocol_state blocks in

  let%await _ =
    Debugger.make blocks protocol_state history
    |> Debugger.use (module Show)
    |> Debugger.use (module Goto)
    |> Debugger.use (module Save)
    |> Debugger.use (module Next)
    |> Debugger.use (module Prev)
    |> Debugger.run in
  await (`Ok ())

let state_bin =
  let state_bin = Cmdliner.Arg.non_dir_file in
  let docv = "state.bin" in
  let doc = "The path to the state.bin file" in
  let open Arg in
  required & pos 0 (some state_bin) None & info [] ~docv ~doc

let blocks =
  let blocks = Cmdliner.Arg.non_dir_file in
  let docv = "database" in
  let doc = "The path to the database folder" in
  let open Arg in
  required & pos 1 (some blocks) None & info [] ~docv ~doc

let run =
  let open Term in
  ret (const Lwt_main.run $ (const debugger $ state_bin $ blocks))

let info =
  let doc = "Deku debugger" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-indexer" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

let _ = Cmd.eval (Cmd.v info run)
