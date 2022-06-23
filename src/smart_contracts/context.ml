module type CTX = sig
  include Conversions.S

  include
    State.S
      with module Address = Address
       and module Amount = Amount
       and module Ticket_id = Ticket_id

  module Addressing : sig
    val sender : #State.addressing -> Address.t

    val source : #State.addressing -> Address.t

    val self : #State.addressing -> Address.t

    val get_contract_opt : #State.addressing -> Address.t -> Address.t option
  end
  module Table_ops : sig
    val read_ticket :
      < State.table_access ; State.addressing ; .. > ->
      Ticket_handle.t ->
      Ticket_id.t * Amount.t * Ticket_handle.t

    val split_ticket :
      < State.table_access ; State.addressing ; .. > ->
      Ticket_handle.t * Amount.t * Amount.t ->
      Ticket_handle.t * Ticket_handle.t

    val join_tickets :
      < State.table_access ; State.addressing ; .. > ->
      Ticket_handle.t * Ticket_handle.t ->
      Ticket_handle.t

    val own_ticket :
      < State.table_access ; State.addressing ; .. > ->
      Ticket_handle.t ->
      Ticket_handle.t

    val mint_ticket :  < State.table_access ; State.addressing ; .. > ->
      (bytes * Amount.t) ->
      Ticket_handle.t
  end

  module Operations : sig
    val transaction :
      < State.table_access ; State.with_operations ; State.addressing ; .. > ->
      bytes * (Ticket_handle.t * Amount.t) * Address.t ->
      int
  end

  val make_state :
    get_contract_opt:(Address.t -> Address.t option) ->
    source:Address.t ->
    sender:Address.t ->
    self:Address.t ->
    contract_owned_tickets:(Ticket_id.t * Amount.t) Seq.t ->
    provided_tickets:(Ticket_id.t * Amount.t) Seq.t ->
    State.full_state
end
