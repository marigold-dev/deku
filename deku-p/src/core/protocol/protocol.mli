open Deku_concepts
open Deku_ledger

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Deku_tezos.Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      vm_state : Deku_gameboy.state;
      game : Game.t;
    }

type t = protocol

val encoding : protocol Data_encoding.t
val initial : ?twitch_oracle_address:Address.t -> unit -> protocol

val prepare :
  parallel:
    ((string -> Operation.Initial.t option) ->
    string list ->
    Operation.Initial.t list) ->
  payload:string list ->
  Operation.Initial.t list

val apply :
  current_level:Level.t ->
  payload:Operation.Initial.t list ->
  tezos_operations:Tezos_operation.t list ->
  protocol ->
  protocol * Receipt.t list * exn list
