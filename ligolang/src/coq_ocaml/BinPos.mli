open Datatypes

module Pos :
 sig
  val succ : Zarith.t -> Zarith.t

  val of_succ_nat : nat -> Zarith.t
 end
