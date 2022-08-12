open Deku_concepts
open Deku_tezos

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      contract_storage : Contract_storage.t;
    }

type t = protocol

val initial : protocol

val apply :
  parallel:((string -> Operation.t option) -> string list -> Operation.t list) ->
  current_level:Level.t ->
  payload:string list ->
  tezos_operations:Tezos_operation.t list ->
  protocol ->
  protocol * Receipt.t list
