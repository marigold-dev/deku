open Deku_concepts
open Deku_tezos

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
    }

type t = protocol

val initial : protocol

val prepare :
  parallel:((string -> Operation.t option) -> string list -> Operation.t list) ->
  payload:string list ->
  Operation.t list

val apply :
  current_level:Level.t ->
  payload:Operation.t list ->
  tezos_operations:Tezos_operation.t list ->
  protocol ->
  protocol * Receipt.t list
