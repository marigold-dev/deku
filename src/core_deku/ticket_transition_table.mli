module Errors : sig
  type t =
    [ `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount ]
  [@@deriving show]
end

type t [@@deriving yojson]

val own :
  t ->
  Address.t ->
  Ticket_handle.t ->
  ( Ticket_handle.t * t,
    [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
  result

val read_ticket :
  t ->
  sender:Address.t ->
  ticket_handle:Ticket_handle.t ->
  ( (Ticket_id.t * Amount.t * Ticket_handle.t) * t,
    [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
  result

val split_ticket :
  t ->
  sender:Address.t ->
  ticket_handle:Ticket_handle.t ->
  amounts:Amount.t * Amount.t ->
  ( (Ticket_handle.t * Ticket_handle.t) * t,
    [> `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount ] )
  result

val join_tickets :
  t ->
  sender:Address.t ->
  handles:Ticket_handle.t * Ticket_handle.t ->
  ( Ticket_handle.t * t,
    [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
  result

val init :
  sender:Address.t ->
  self:Address.t ->
  tickets:(Ticket_id.t * Amount.t) Seq.t ->
  temporary_tickets:(Ticket_id.t * Amount.t) Seq.t ->
  t

val finalize : t -> (Ticket_id.t * Amount.t) Seq.t
