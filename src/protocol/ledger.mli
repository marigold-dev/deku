open Deku_concepts
open Deku_crypto

type ledger = private Amount.t Address.Map.t
type t = ledger

val initial : ledger
val balance : Address.t -> ledger -> Amount.t
val deposit : Address.t -> Amount.t -> ledger -> ledger

val transfer :
  sender:Address.t -> receiver:Address.t -> Amount.t -> ledger -> ledger option

module Withdrawal_handle_hash : sig
  type t = BLAKE2b.t [@@deriving yojson]
end
