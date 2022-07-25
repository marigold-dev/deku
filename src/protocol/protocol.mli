open Deku_concepts

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      ledger : Ledger.t;
    }

type t = protocol

val initial : protocol

val apply :
  parallel:((string -> Operation.t option) -> string list -> Operation.t list) ->
  current_level:Level.t ->
  payload:string list ->
  protocol ->
  protocol
