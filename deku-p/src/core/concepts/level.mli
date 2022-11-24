open Deku_stdlib

type level
type t = level [@@deriving show, eq, ord]

val zero : level
val next : level -> level

(* repr *)
val of_n : N.t -> level
val to_n : level -> N.t
val encoding : level Data_encoding.t

(* operations *)
val ( > ) : level -> level -> bool

module Map : Map.S with type key = level
