type nat
type t = nat [@@deriving show, eq, ord, yojson]

exception Not_a_natural

val zero : nat
val one : nat

(* repr *)
val of_z : Z.t -> nat option
val to_z : nat -> Z.t

(* operations *)
val ( + ) : nat -> nat -> nat
val ( - ) : nat -> nat -> nat option
val ( < ) : nat -> nat -> bool