open BinPos
open Datatypes

module Z =
 struct
  type t = Zarith.t

  (** val of_nat : nat -> Zarith.t **)

  let of_nat = function
  | O -> Zarith.zero
  | S n0 -> (Pos.of_succ_nat n0)
 end
