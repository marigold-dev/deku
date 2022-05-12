module type S = sig
  module Types : Types_intf.S

  include module type of Types

  module State : sig
    class virtual finalization :
      object
        method virtual finalize :
          int list ->
          ( (ticket_repr * amount) Seq.t * operation list,
            [`Execution_error] )
          result
      end

    class virtual table_access :
      object
        method virtual table : table
      end

    class virtual addressing :
      object
        method virtual self : address

        method virtual sender : address

        method virtual source : address

        method virtual get_contract_opt : address -> address option
      end

    class virtual with_operations :
      object
        method virtual add_operation : operation -> int
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

module Make (CC : Conv.S) :
  S
    with type address = CC.Address.t
     and type table = CC.Table.t
     and type ticket_repr = CC.Ticket_id.t
     and type ticket_handle = CC.Ticket_handle.t
     and type operation = CC.Operation.t
     and type amount = CC.Amount.t
