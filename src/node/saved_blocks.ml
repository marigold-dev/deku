open Protocol
open Helpers

type t = {
  recent_blocks: (float * Block.t) list;
  path: string
}

let slice_length = 100L

let make path =
  { recent_blocks = []; path }

let is_same_range blocks n =
  (* Assumes that blocks is ordered and checks only the first one *)
  match blocks with
    | [] -> false
    | (_, head_block)::_ ->
      Int64.div head_block.Block.block_height slice_length = Int64.div n slice_length

let save_block t block =
  let x = (Unix.time (), block) in
  if is_same_range t.recent_blocks block.Block.block_height then
    { t with recent_blocks = x :: t.recent_blocks }
  else
    (Lwt.async (fun () ->
      let bin = Marshal.to_string t.recent_blocks [] in
      let hundredth = Int64.div block.Block.block_height slice_length in
      let path = Printf.sprintf "%s/blocks-%Ld" t.path hundredth in
      Lwt_io.with_file ~mode:Output path (fun oc ->
        let%await () = Lwt_io.write oc bin in
        Lwt_io.flush oc));
    { t with recent_blocks = [x] })

let find_block n blocks =
  List.find_opt
    (fun (_, block) ->
      Int64.equal block.Block.block_height n)
    blocks

let find_saved_block t n =
  let hundredth = Int64.(div n slice_length |> add 1L) in
  let path = Printf.sprintf "%s/blocks-%Ld" t.path hundredth in
  try
    In_channel.open_bin path
    |> Marshal.from_channel
    |> find_block n
  with
    _ ->
    Log.debug "Failed to read block %Ld from disk" n;
    None

let get_block t n =
  if is_same_range t.recent_blocks n then
    find_block n t.recent_blocks
  else
    find_saved_block t n
