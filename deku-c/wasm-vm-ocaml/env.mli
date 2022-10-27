module Contract_address = Deku_ledger.Contract_address
module Address = Deku_ledger.Address
module Ledger = Deku_ledger.Ledger
open Deku_crypto
open Deku_stdlib

type t = {
  sender : Address.address;
  source : Address.address;
  mutable ledger : Ledger.ledger;
  mutable state : State.t;
  mutable ticket_table : Ticket_table.t;
}

val make :
  state:State.t ->
  ledger:Ledger.ledger ->
  sender:Address.address ->
  source:Address.address ->
  t

val execute :
  t ->
  operation_hash:BLAKE2b.t ->
  tickets:(Deku_ledger.Ticket_id.t * N.t) list ->
  operation:Operations.t ->
  (t, string) result

val finalize : (t, string) result -> (State.t * Ledger.ledger, string) result
