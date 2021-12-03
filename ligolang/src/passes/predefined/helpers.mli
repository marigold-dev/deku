module Stacking : sig
  open Tezos_utils.Michelson

  type predicate
  val simple_constant : unit t -> predicate
  val simple_unary : unit t -> predicate
  val simple_binary : unit t -> predicate
  val simple_ternary : unit t -> predicate
  val simple_tetrary : unit t -> predicate
  val simple_pentary : unit t -> predicate
  val simple_hexary : unit t -> predicate

  val trivial_special : string -> predicate
  val special : ((string -> unit michelson) -> unit michelson) -> predicate

  val unpredicate :
    Location.t ->
    (string -> unit michelson) ->
    predicate -> Location.t michelson
end
