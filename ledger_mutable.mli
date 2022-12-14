open Deku_crypto
open Deku_concepts
open Deku_tezos

exception Ledger_limit
exception Invalid_owner
exception Invalid_code
exception Ticket_mismatch
exception Insufficient_funds

type ledger
type t = ledger

val create : size:int -> ledger
(** [create ~size] *)

val owner : ledger -> Ledger_code.t -> Contract.t
(** [owner ledger code]

  @raise Invalid_code
*)

val balance : ledger -> Ledger_code.t -> Amount.t
(** [balance ledger code]

  @raise Invalid_code
*)

val transfer :
  ledger ->
  owner:Contract.t ->
  source:Ledger_code.t ->
  destination:Ledger_code.t ->
  Amount.t ->
  unit
(** [transfer ledger ~owner ~source ~destination amount]

  @raise Invalid_code
  @raise Invalid_owner
  @raise Insufficient_funds
*)

val deposit :
  ledger ->
  expected_ticket:Ticket_id.t ->
  destination:Ledger_code.t ->
  Amount.t ->
  unit
(** [deposit ledger ~destination amount]

  @raise Invalid_code
*)

val withdraw :
  ledger ->
  deku_ticketer:Tezos_contract_hash.t ->
  owner:Contract.t ->
  source:Ledger_code.t ->
  destination:Tezos_contract.t ->
  Amount.t ->
  Burn_receipt.t
(** [burn ledger ~source ~ticket amount]

  @raise Invalid_code
  @raise Invalid_owner
  @raise Insufficient_funds
*)

val register : ledger -> owner:Contract.t -> ticket:Ticket_id.t -> Ledger_code.t
(** [register ledger ~owner ~ticket]
  
  @raise Ledger_limit *)

val find_burn_receipt :
  id:int -> ledger -> (Burn_receipt.t * (BLAKE2b.t * BLAKE2b.t) list) option

val burn_receipt_root : ledger -> BLAKE2b.t
val encoding : ledger Data_encoding.t
