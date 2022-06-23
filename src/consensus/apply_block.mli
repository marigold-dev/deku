val apply_block :
  block:Protocol.Block.t ->
  State.t ->
  (* TODO: snapshot_ref is here because currently snapshotting is really bad *)
  ( State.t * Snapshots.snapshot_ref option * Steps.t,
    [> `Invalid_block_when_applying] )
  result
