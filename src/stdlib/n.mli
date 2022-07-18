type nat
type t = nat [@@deriving show, eq, ord]

val zero : nat
val one : nat
val of_z : Z.t -> nat option
val to_z : nat -> Z.t
val ( + ) : nat -> nat -> nat
val ( - ) : nat -> nat -> nat option
val ( < ) : nat -> nat -> bool
