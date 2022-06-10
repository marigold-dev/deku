open Helpers
open Crypto

type t = {
  ledger : Ledger.t;
  contract_storage : Contract_storage.t;
}
[@@deriving yojson]

type receipt = Receipt_tezos_withdraw of Ledger.Withdrawal_handle.t
[@@deriving yojson]

let empty = { ledger = Ledger.empty; contract_storage = Contract_storage.empty }

let ledger t = t.ledger

let contract_storage t = t.contract_storage

let hash t = to_yojson t |> Yojson.Safe.to_string |> BLAKE2B.hash

let apply_tezos_operation t tezos_operation =
  let open Tezos_operation in
  let apply_internal_operation t internal_operation =
    let { ledger; contract_storage } = t in
    match internal_operation with
    | Tezos_deposit { destination; amount; ticket } ->
      let ledger =
        match destination with
        | Implicit key_hash ->
          let destination = key_hash in
          Ledger.deposit destination amount ticket ledger
        | Originated _ -> failwith "not implemented" in
      { ledger; contract_storage } in
  let { hash = _; payload } = tezos_operation in
  let { tezos_operation_hash = _; internal_operations } = payload in
  List.fold_left apply_internal_operation t internal_operations

let apply_user_operation t operation_hash user_operation =
  let open User_operation in
  let { source; initial_operation; hash = _ } = user_operation in
  let { ledger; contract_storage } = t in
  match initial_operation with
  | Transaction { destination; amount; ticket } ->
    let%ok ledger =
      Ledger.transfer
        ~sender:(Address.of_key_hash source)
        ~destination amount ticket ledger in
    Ok ({ contract_storage; ledger }, None)
  | Tezos_withdraw { owner; amount; ticket } ->
    let%ok ledger, handle =
      Ledger.withdraw ~sender:source ~destination:owner amount ticket ledger
    in
    Ok ({ ledger; contract_storage }, Some (Receipt_tezos_withdraw handle))
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
    let contract_storage =
      Contract_storage.originate_contract t.contract_storage ~address ~contract
    in
    Ok ({ contract_storage; ledger }, None)
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
      Contract_storage.get_contract t.contract_storage ~address:to_invoke
      |> Option.to_result ~none:"Contract not found"
      |> wrap_error in
    let%ok contract, _user_op_list =
      Contract_vm.Interpreter.invoke ~source ~arg:argument ~gas:initial_gas
        contract
      |> wrap_error in
    let contract_storage =
      Contract_storage.update_contract_storage contract_storage
        ~address:to_invoke ~updated_contract:contract in
    Ok ({ ledger; contract_storage }, None)

let apply_user_operation t hash user_operation =
  match apply_user_operation t hash user_operation with
  | Ok (t, receipt) -> (t, receipt)
  (* TODO: use this erros for something *)
  | Error (`Origination_error _ | `Invocation_error _) -> (t, None)
  | Error `Insufficient_funds -> (t, None)
