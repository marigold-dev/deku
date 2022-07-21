open Deku_concepts

type ledger = private Amount.t Address.Map.t
type t = ledger

val initial : ledger
val balance : Address.t -> ledger -> Amount.t
val deposit : Address.t -> Amount.t -> ledger -> ledger

val transfer :
  sender:Address.t -> receiver:Address.t -> Amount.t -> ledger -> ledger option
