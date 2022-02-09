open Crypto
open Protocol
type snapshot = {
  hash: BLAKE2B.t ;
  data: string }[@@deriving yojson]
type t = private
  {
  current_snapshot: snapshot ;
  next_snapshots: (int64 * snapshot) list ;
  last_block: Block.t ;
  last_block_signatures: Signatures.t ;
  additional_blocks: Block.t list }
val make :
  initial_snapshot:snapshot ->
    initial_block:Block.t -> initial_signatures:Signatures.t -> t
val append_block : pool:Block_pool.t -> (Block.t * Signatures.t) -> t -> t
[@@ocaml.doc " this should be called when a block is received "]
val add_snapshot : new_snapshot:snapshot -> block_height:int64 -> t -> t
val start_new_epoch : t -> t
val get_next_snapshot : t -> snapshot option