open Helpers
open Core_deku

type t =
  | Contract_invocation  of {
      to_invoke : Contract_address.t;
      argument : Contract_vm.Invocation_payload.t;
    }
  | Contract_origination of Contract_vm.Origination_payload.t
[@@deriving yojson]

let transaction storage sender operation_hash operation =
  match operation with
  | Contract_origination to_originate ->
    (* @TODO: deduct gas from account and check *)
    let balance = Int.max_int |> Amount.of_int in
    (* TODO: those constants should not be defined here *)
    let origination_cost = 250 |> Amount.of_int in
    let%assert () =
      Amount.
        ( `Origination_error "Not enought funds",
          let comparison_result = compare balance origination_cost in
          comparison_result >= 0 ) in

    let initial_gas = Amount.to_int Amount.(balance - origination_cost) in
    let wrap_error t = t |> Result.map_error (fun x -> `Origination_error x) in
    (* TODO: Burn on storage size change, need CTEZ *)
    let%ok contract =
      Contract_vm.Compiler.compile ~gas:initial_gas to_originate |> wrap_error
    in
    let address = Contract_address.of_user_operation_hash operation_hash in
    Contract_storage.originate_contract storage ~address ~contract;
    Ok ()
  | Contract_invocation { to_invoke; argument } ->
    let balance = max_int |> Amount.of_int in
    (* TODO: find good transaction cost *)
    let invocation_cost = 250 |> Amount.of_int in
    let%assert () =
      Amount.
        ( `Invocation_error "Not enought funds",
          let comparison_result = compare balance invocation_cost in
          comparison_result >= 0 ) in

    let burn_cap = invocation_cost in
    let initial_gas = Amount.(to_int (balance - burn_cap)) in
    let wrap_error t = Result.map_error (fun x -> `Invocation_error x) t in
    let%ok contract =
      Contract_storage.get_contract storage ~address:to_invoke
      |> Option.to_result ~none:"Contract not found"
      |> wrap_error in
    let%ok contract, _user_op_list =
      Contract_vm.Interpreter.invoke ~source:sender ~arg:argument
        ~gas:initial_gas contract
      |> wrap_error in
    Contract_storage.update_contract_storage storage ~address:to_invoke
      ~updated_contract:contract;
    Ok ()
