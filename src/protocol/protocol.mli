open Deku_concepts
open Deku_tezos

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      receipts : Receipt.t Operation_hash.Map.t;
    }

type t = protocol

val initial : protocol

val find_withdraw_proof :
  operation_hash:Operation_hash.t ->
  t ->
  ( Ledger.withdraw_handle * Ledger.withdraw_proof,
    [> `Unknown_operation ] )
  Result.t

val apply :
  parallel:((string -> Operation.t option) -> string list -> Operation.t list) ->
  current_level:Level.t ->
  payload:string list ->
  tezos_operations:Tezos_operation.t list ->
  protocol ->
  protocol * Receipt.t list
