open Deku_concepts
open Deku_tezos
open Deku_external_vm
open Deku_crypto

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      vm_state : External_vm_protocol.State.t;
      receipts : Receipt.t Operation_hash.Map.t;
      chain_id : Deku_tezos.Address.t;
    }

type t = protocol [@@deriving yojson]

val initial : chain_id:Deku_tezos.Address.t -> protocol

val initial_with_vm_state :
  vm_state:External_vm_protocol.State.t ->
  chain_id:Deku_tezos.Address.t ->
  protocol

val prepare :
  parallel:((string -> Operation.t option) -> string list -> Operation.t list) ->
  payload:string list ->
  Operation.t list

val find_withdraw_proof :
  operation_hash:Operation_hash.t ->
  t ->
  ( Ledger.withdrawal_handle * Ledger.withdraw_proof * BLAKE2b.t,
    [> `Unknown_operation ] )
  Result.t

val apply :
  current_level:Level.t ->
  payload:Operation.t list ->
  tezos_operations:Tezos_operation.t list ->
  protocol ->
  protocol * Receipt.t list * exn list
