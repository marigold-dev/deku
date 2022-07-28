open Deku_crypto
open Deku_concepts

type consensus = private
  | Consensus of {
      validators : Validators.t;
      current_level : Level.t;
      current_block : Block_hash.t;
      last_block_author : Key_hash.t;
      last_block_update : Timestamp.t option;
    }

and t = consensus

val make : validators:Validators.t -> block:Block.t -> consensus

(* transition *)
val apply_block : current:Timestamp.t -> block:Block.t -> consensus -> consensus

(* judging *)
val is_valid : block:Block.t -> consensus -> bool

val is_expected_author :
  current:Timestamp.t -> author:Key_hash.t -> consensus -> bool
