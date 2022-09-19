open Deku_concepts
open Deku_tezos
open Deku_external_vm

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      vm_state : External_vm_protocol.State.t;
    }

type t = protocol

val initial : protocol
val initial_with_vm_state : vm_state:External_vm_protocol.State.t -> protocol

val apply :
  parallel:((string -> Operation.t option) -> string list -> Operation.t list) ->
  current_level:Level.t ->
  payload:string list ->
  tezos_operations:Tezos_operation.t list ->
  protocol ->
  protocol * Receipt.t list
