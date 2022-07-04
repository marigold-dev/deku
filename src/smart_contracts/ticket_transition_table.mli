module Errors : sig
  type t =
    [ `Attempted_to_merge_different_tickets
    | `Insufficient_funds
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount ]
  [@@deriving show]
end

module type S = sig
  include Conversions.S

  type t

  val own :
    t ->
    Address.t ->
    Ticket_handle.t ->
    ( Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val mint_ticket :
    t -> sender:Address.t -> amount:Amount.t -> bytes -> Ticket_handle.t

  val read_ticket :
    t ->
    sender:Address.t ->
    ticket_handle:Ticket_handle.t ->
    ( Ticket_id.t * Amount.t * Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val split_ticket :
    t ->
    sender:Address.t ->
    ticket_handle:Ticket_handle.t ->
    amounts:Amount.t * Amount.t ->
    ( Ticket_handle.t * Ticket_handle.t,
      [> `Ticket_doesnt_exist
      | `Ticket_ownership_violation
      | `Ticket_split_invalid_amount ] )
    result

  val join_tickets :
    t ->
    sender:Address.t ->
    handles:Ticket_handle.t * Ticket_handle.t ->
    ( Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val init :
    self:Address.t ->
    mapping:((Ticket_id.t * Amount.t) * Ticket_handle.t) list ->
    tickets:(Ticket_id.t * Amount.t) Seq.t ->
    temporary_tickets:
      ((Ticket_id.t * Amount.t) * (Ticket_handle.t * Int64.t option)) Seq.t ->
    t * (Int64.t option * Ticket_handle.t) list option

  val finalize : t -> (Ticket_handle.t * Ticket_id.t * Amount.t) Seq.t
end

module Make (CC : Conversions.S) :
  S
    with type Address.t = CC.Address.t
     and type Amount.t = CC.Amount.t
     and type Ticket_id.t = CC.Ticket_id.t
