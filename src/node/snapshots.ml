open Crypto
open Protocol
open Helpers
type snapshot = {
  hash : BLAKE2B.t;
  data : string;
}
[@@deriving yojson]
type t = {
  current_snapshot : snapshot;
  next_snapshots : (int64 * snapshot) list;
  last_block : Block.t;
  last_block_signatures : Signatures.t;
  additional_blocks : Block.t list;
}
let make ~initial_snapshot ~initial_block ~initial_signatures =
  {
    current_snapshot = { hash = initial_block.Block.state_root_hash; data = "" };
    next_snapshots = [(initial_block.block_height, initial_snapshot)];
    last_block = initial_block;
    last_block_signatures = initial_signatures;
    additional_blocks = [];
  }
let append_block ~pool (block, signatures) t =
  if t.last_block.block_height > block.Block.block_height then
    t
  else
    let blocks, (block, signatures) =
      Block_pool.find_all_signed_blocks_above (block, signatures) pool in
    {
      current_snapshot = t.current_snapshot;
      next_snapshots = t.next_snapshots;
      last_block = block;
      last_block_signatures = signatures;
      additional_blocks = blocks @ [t.last_block] @ t.additional_blocks;
    }
let add_snapshot ~new_snapshot ~block_height t =
  Format.eprintf "\027[36mNew protocol snapshot hash: %s\027[m\n%!"
    (new_snapshot.hash |> BLAKE2B.to_string);
  {
    next_snapshots = t.next_snapshots @ [(block_height, new_snapshot)];
    current_snapshot = t.current_snapshot;
    last_block = t.last_block;
    last_block_signatures = t.last_block_signatures;
    additional_blocks = t.additional_blocks;
  }
let start_new_epoch t =
  let rec truncate_additional_blocks block_height blocks =
    match blocks with
    | hd :: tl when hd.Block.block_height > block_height ->
      hd :: truncate_additional_blocks block_height tl
    | _ -> [] in
  match t.next_snapshots with
  | (height, snapshot) :: tl ->
    let additional_blocks =
      truncate_additional_blocks height t.additional_blocks in
    {
      current_snapshot = snapshot;
      next_snapshots = tl;
      last_block = t.last_block;
      last_block_signatures = t.last_block_signatures;
      additional_blocks;
    }
  | [] -> failwith "You must add a snapshot before you can start a new epoch"
let get_next_snapshot t =
  let%some _, snapshot = List.nth_opt t.next_snapshots 0 in
  Some snapshot
