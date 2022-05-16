open Crypto
open Protocol
open Helpers
type snapshot = Network.Protocol_snapshot.snapshot = {
  hash : BLAKE2B.t;
  data : string;
}
[@@deriving yojson]
type snapshot_ref = snapshot option Atomic.t
type t = {
  current_snapshot : snapshot_ref;
  next_snapshots : (int64 * snapshot_ref) list;
  last_block : Block.t;
  last_block_signatures : Signatures.t;
  additional_blocks : Block.t list;
}
let make ~initial_snapshot ~initial_block ~initial_signatures =
  {
    current_snapshot =
      Atomic.make
        (* TODO: if a snapshot is requested before first epoch starts, we will send meaningless data.
           We need to have logic in the snapshot request handler such that we send a 503 error or something
           instead of sending bad data. *)
        (Some { hash = initial_block.Block.state_root_hash; data = "" });
    next_snapshots =
      [(initial_block.block_height, Atomic.make (Some initial_snapshot))];
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
let add_snapshot_ref ~block_height t =
  let atom = Atomic.make None in
  ( atom,
    {
      next_snapshots = t.next_snapshots @ [(block_height, atom)];
      current_snapshot = t.current_snapshot;
      last_block = t.last_block;
      last_block_signatures = t.last_block_signatures;
      additional_blocks = t.additional_blocks;
    } )
let set_snapshot_ref ref_ snapshot =
  Logs.info (fun m -> m "New protocol snapshot hash: %s"
    (snapshot.hash |> BLAKE2B.to_string));
  Atomic.set ref_ (Some snapshot)
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

(* THIS IS GLOBAL STATE OUTSIDE OF ./server.ml. CAUTION *)
let latest_finished_snapshot = ref None

let get_most_recent_snapshot t =
  match Atomic.get t.current_snapshot with
  | Some snapshot ->
    latest_finished_snapshot := Some snapshot;
    Ok snapshot
  | None ->
  match !latest_finished_snapshot with
  | Some latest_finished_snapshot -> Ok latest_finished_snapshot
  | None -> Error `Node_not_yet_initialized

let get_next_snapshot t =
  let%some _, snapshot = List.nth_opt t.next_snapshots 0 in
  Atomic.get snapshot
