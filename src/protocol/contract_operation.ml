open Deku_concepts
open Contracts

type operation =
  | Contract_invocation of {
      to_invoke : Contract_address.t;
      argument : Contract_vm.Invocation_payload.t;
      (*TODO: remove tickets field when we will do normal parsing of arguments from the cli *)
      tickets :
        ((Ticket_id.t * Amount.t)
        * (Smart_contracts.Ticket_handle.t * int64 option))
        list;
    }
  | Contract_origination of {
      payload : Contract_vm.Origination_payload.t;
      (*TODO: remove tickets field when we will do normal parsing of initial storage from the cli *)
      tickets :
        ((Ticket_id.t * Amount.t)
        * (Smart_contracts.Ticket_handle.t * int64 option))
        list;
    }
[@@deriving yojson]

type t = operation [@@deriving yojson]
