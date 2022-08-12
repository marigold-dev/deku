type operation =
  | Contract_invocation of {
      to_invoke : Contract_address.t;
      argument : Contracts.Contract_vm.Invocation_payload.t;
      tickets :
        ((Ticket_id.ticket_id * Deku_concepts.Amount.amount)
        * (int32 * int64 option))
        list;
    }
  | Contract_origination of {
      payload : Contracts.Contract_vm.Origination_payload.t;
      tickets :
        ((Ticket_id.ticket_id * Deku_concepts.Amount.amount)
        * (int32 * int64 option))
        list;
    }

type t = operation [@@deriving yojson]
