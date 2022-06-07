val apply_block :
  block:Protocol.Block.t ->
  State.t ->
  (State.t * Steps.t, [> `Invalid_block_when_applying]) result
