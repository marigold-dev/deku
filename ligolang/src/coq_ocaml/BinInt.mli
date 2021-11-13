open BinPos
open Datatypes

module Z :
 sig
  type t = Zarith.t

  val of_nat : nat -> Zarith.t
 end
