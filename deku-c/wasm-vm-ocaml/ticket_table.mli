open Deku_stdlib

exception
  Table of
    [ `Invalid_split_amount
    | `Joined_different_tickets
    | `Ownership_violation
    | `Ticket_doesnt_exist ]

val show_error :
  [ `Invalid_split_amount
  | `Joined_different_tickets
  | `Ownership_violation
  | `Ticket_doesnt_exist ] ->
  string

module Entry : sig
  type t = {
    ticket_id : Deku_ledger.Ticket_id.t;
    amount : N.t;
    mutable alive : bool;
  }
  [@@deriving ord, show]

  val assert_not_dead : t -> unit
  val join : t -> t -> t
  val split : t -> N.t * N.t -> t * t
end

module Handle : sig
  type t = int
end

type t

val add : t -> Entry.t -> Handle.t
val extract : t -> Handle.t -> Entry.t
val unsafe_read : t -> Handle.t -> Entry.t
val mint_ticket : t -> Deku_ledger.Ticket_id.t -> N.t -> Handle.t
val read_ticket : t -> Handle.t -> Deku_ledger.Ticket_id.t * N.t * Handle.t
val split_ticket : t -> Handle.t -> N.t * N.t -> (Handle.t * Handle.t) option
val join_tickets : t -> Handle.t * Handle.t -> Handle.t option
val reset : t -> unit
val init : Entry.t list -> t
