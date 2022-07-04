module type S = sig
  include Conversions.S

  module Ticket_handle = Ticket_handle

  module Ticket_transition_table :
    Ticket_transition_table.S
      with type Address.t = Address.t
       and type Amount.t = Amount.t
       and type Ticket_id.t = Ticket_id.t

  module Operation :
    Operation.S
      with type Address.t = Address.t
       and type Amount.t = Amount.t
       and type Ticket_id.t = Ticket_id.t

  module State : sig
    class virtual finalization :
      object
        method virtual finalize :
          int list ->
          ( ((Ticket_id.t * Amount.t) * Ticket_handle.t) List.t
            * (Ticket_id.t * Amount.t) Seq.t
            * Operation.t list,
            [`Execution_error] )
          result
      end

    class virtual table_access :
      object
        method virtual table : Ticket_transition_table.t
      end

    class virtual addressing :
      object
        method virtual self : Address.t

        method virtual sender : Address.t

        method virtual source : Address.t

        method virtual get_contract_opt : Address.t -> Address.t option
      end

    class virtual with_operations :
      object
        method virtual add_operation : Operation.t -> int
      end

    class virtual full_state :
      object
        inherit table_access

        inherit finalization

        inherit addressing

        inherit with_operations
      end
  end
end

module Make (CC : Conversions.S) :
  S
    with type Address.t = CC.Address.t
     and type Amount.t = CC.Amount.t
     and type Ticket_id.t = CC.Ticket_id.t
