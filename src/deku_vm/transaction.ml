type t =
  | Contract_invocation  of {
      to_invoke : Contract_address.t;
      argument : Contract_vm.Invocation_payload.t;
    }
  | Contract_origination of Contract_vm.Origination_payload.t
