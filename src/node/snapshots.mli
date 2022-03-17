open Crypto
open Protocol
type snapshot = {
  hash : BLAKE2B.t;
  data : string;
}
[@@deriving yojson]
type snapshot_ref
type t = private {
  current_snapshot : snapshot_ref;
  next_snapshots : (int64 * snapshot_ref) list;
  last_block : Block.t;
  last_block_signatures : Signatures.t;
  additional_blocks : Block.t list;
}
val make :
  initial_snapshot:snapshot ->
  initial_block:Block.t ->
  initial_signatures:Signatures.t ->
  t

val append_block : pool:Block_pool.t -> Block.t * Signatures.t -> t -> t
  [@@ocaml.doc " this should be called when a block is received "]

val add_snapshot_ref : block_height:int64 -> t -> snapshot_ref * t
val set_snapshot_ref : snapshot_ref -> snapshot -> unit
val start_new_epoch : t -> t
val get_current_snapshot : t -> snapshot option
val get_next_snapshot : t -> snapshot option
