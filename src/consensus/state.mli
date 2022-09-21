open Deku_crypto
open Deku_concepts

type state = private
  | State of {
      validators : Validators.t;
      current_level : Level.t;
      current_block : Block_hash.t;
      last_block_author : Key_hash.t;
      last_block_update : Timestamp.t;
    }

and t = state

val genesis : validators:Validators.t -> state

(* read *)
val current_author : current:Timestamp.t -> state -> Key_hash.t

(* update *)
val apply_block : current:Timestamp.t -> block:Block.t -> state -> state

val apply_bootstrap_signal :
  current:Timestamp.t -> author:Key_hash.t -> state -> state option
