open Deku_concepts

exception Ledger_limit
exception Invalid_code
exception Balance_not_enough
exception Owner_mismatch
exception Ticket_mismatch

type ledger
type t = ledger

val initial : ledger

val owner : Ledger_code.t -> ledger -> Contract.t
(** [owner code ledger]

  @raise Invalid_code *)

val ticket : Ledger_code.t -> ledger -> Ticket_id.t
(** [balance code ledger]

  @raise Invalid_code *)

val balance : Ledger_code.t -> ledger -> Amount.t
(** [balance code ledger]

  @raise Invalid_code *)

val burn :
  owner:Contract.t -> source:Ledger_code.t -> Amount.t -> ledger -> ledger
(** [transfer ~owner ~source ~destination amount ledger]

  @raise Invalid_code
  @raise Owner_mismatch
  @raise Balance_not_enough *)

val deposit :
  ticket:Ticket_id.t ->
  destination:Ledger_code.t ->
  Amount.t ->
  ledger ->
  ledger
(** [deposit ~ticket ~destination amount ledger]

  @raise Invalid_code
  @raise Ticket_mismatch *)

val transfer :
  owner:Contract.t ->
  source:Ledger_code.t ->
  destination:Ledger_code.t ->
  Amount.t ->
  ledger ->
  ledger
(** [transfer ~owner ~source ~destination amount ledger]

  @raise Invalid_code
  @raise Owner_mismatch
  @raise Balance_not_enough
  @raise Ticket_mismatch *)

val register :
  owner:Contract.t -> ticket:Ticket_id.t -> ledger -> ledger * Ledger_code.t
(** [register ~owner ~ticket ledger]
  
  @raise Ledger_limit *)

(* repr *)
val encoding : ledger Data_encoding.t
