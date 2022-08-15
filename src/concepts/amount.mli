open Deku_stdlib

type amount
type t = amount [@@deriving show, eq, ord, yojson]

val zero : amount
val one : amount
val of_n : N.t -> amount
val to_n : amount -> N.t
val ( + ) : amount -> amount -> amount
val ( - ) : amount -> amount -> amount option
val of_int : int -> amount option