open Helpers
open Crypto
type t = {
  ledger : Ledger.t;
  vm_state : Go_vm.t;
}
[@@deriving yojson]
type receipt = Receipt_tezos_withdraw of Ledger.Withdrawal_handle.t
[@@deriving yojson]
let empty = { ledger = Ledger.empty; vm_state = Go_vm.empty }
let ledger t = t.ledger
let hash t = to_yojson t |> Yojson.Safe.to_string |> BLAKE2B.hash
let apply_tezos_operation t tezos_operation =
  let open Tezos_operation in
  let apply_internal_operation t internal_operation =
    let { ledger; vm_state } = t in
    match internal_operation with
    | Tezos_deposit { destination; amount; ticket } ->
      let ledger =
        match destination with
        | Implicit key_hash ->
          let destination = key_hash in
          Ledger.deposit destination amount ticket ledger
        | Originated _ -> failwith "not implemented" in
      { ledger; vm_state } in
  let { hash = _; payload } = tezos_operation in
  let { tezos_operation_hash = _; internal_operations } = payload in
  List.fold_left apply_internal_operation t internal_operations
let apply_user_operation t user_operation =
  let open User_operation in
  let { hash = _; sender; initial_operation } = user_operation in
  match (Address.to_key_hash sender, initial_operation) with
  | Some sender, Transaction { destination; amount; ticket } ->
    let%ok ledger =
      Ledger.transfer ~sender ~destination amount ticket t.ledger in
    Ok ({ ledger; vm_state = t.vm_state }, None)
  | None, Vm_transaction _
  | None, Transaction _ ->
    Error `Transaction_sender_must_be_implicit_account
  | Some sender, Tezos_withdraw { owner; amount; ticket } ->
    let%ok ledger, handle =
      Ledger.withdraw ~sender ~destination:owner amount ticket t.ledger in
    Ok ({ ledger; vm_state = t.vm_state }, Some (Receipt_tezos_withdraw handle))
  | Some _sender, Vm_transaction { payload } ->
    let vm_state = Go_vm.apply_vm_operation t.vm_state payload in
    Ok ({ ledger = t.ledger; vm_state }, None)
  | None, Tezos_withdraw _ -> Error `Withdraw_sender_must_be_implicit_account
let apply_user_operation t user_operation =
  match apply_user_operation t user_operation with
  | Ok (t, receipt) -> (t, receipt)
  | Error
      ( `Not_enough_funds | `Transaction_sender_must_be_implicit_account
      | `Withdraw_sender_must_be_implicit_account ) ->
    (t, None)
