module type CTX = sig
  module Conv : Conv.S

  include module type of State.Make (Conv)

  module Addressing : sig
    val sender : #State.addressing -> address

    val source : #State.addressing -> address

    val self : #State.addressing -> address

    val get_contract_opt : #State.addressing -> address -> address option
  end

  module Table_ops : sig
    val read_ticket :
      < State.table_access ; State.addressing ; .. > ->
      ticket_handle ->
      ticket_repr * amount * ticket_handle

    val split_ticket :
      < State.table_access ; State.addressing ; .. > ->
      ticket_handle * amount * amount ->
      ticket_handle * ticket_handle

    val join_tickets :
      < State.table_access ; State.addressing ; .. > ->
      ticket_handle * ticket_handle ->
      ticket_handle

    val own_ticket :
      < State.table_access ; State.addressing ; .. > ->
      ticket_handle ->
      ticket_handle
  end

  module Operations : sig
    val transaction :
      < State.table_access ; State.with_operations ; State.addressing ; .. > ->
      bytes * (ticket_handle * amount) * address ->
      int
  end
end

module State = State.Make

module type Conv = Conv.S
