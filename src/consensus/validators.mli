open Deku_crypto

(* TODO: should we care about the adding validator order *)
type validators
type t = validators

(* repr *)
val of_key_hash_list : Key_hash.t list -> validators

(* operations *)
val cardinal : validators -> int
val mem : Key_hash.t -> validators -> bool
val skip : after:Key_hash.t -> skip:int -> validators -> Key_hash.t
