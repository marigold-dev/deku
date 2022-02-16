open Crypto
open Protocol
type snapshot = {
  hash : BLAKE2B.t;
  data : string;
}
[@@deriving yojson]
type t = private {
  current_snapshot : snapshot;
  next_snapshots : (int64 * snapshot) list;
  last_block : Block.t;
  last_block_signatures : Protocol.Signature.t list;
  additional_blocks : Block.t list;
}
val make :
  initial_snapshot:snapshot ->
  initial_block:Block.t ->
  initial_signatures:Protocol.Signature.t list ->
  t
val append_block : Block.t -> Protocol.Signature.t list -> t -> t
  [@@ocaml.doc " this should be called when a block is received "]
val add_snapshot : new_snapshot:snapshot -> block_height:int64 -> t -> t
val start_new_epoch : t -> t
val get_next_snapshot : t -> snapshot option
