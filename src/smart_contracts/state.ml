open Conversions

module State = struct
  class virtual finalization =
    object
      method virtual finalize
          : int list ->
            ( ((Ticket_id.t * Amount.t) * Ticket_handle.t) List.t
              * (Ticket_id.t * Amount.t) Seq.t
              * Operation.t list,
              [`Execution_error] )
            result
    end

  class virtual table_access =
    object
      method virtual table : Ticket_transition_table.t
    end

  class virtual addressing =
    object
      method virtual sender : Address.t

      method virtual self : Address.t

      method virtual source : Address.t

      method virtual get_contract_opt : Address.t -> Address.t option
    end

  class virtual with_operations =
    object
      method virtual add_operation : Operation.t -> int
    end

  class virtual full_state =
    object
      inherit finalization

      inherit table_access

      inherit addressing

      inherit with_operations
    end
end
