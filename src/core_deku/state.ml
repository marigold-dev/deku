open Helpers
open Crypto

type t = {
  ledger : Ledger.t;
  (* contract_storage : Contract_storage.t; *)
  vm_state : External_vm.External_vm_protocol.State.t;
}
[@@deriving yojson]

type receipt = Receipt_tezos_withdraw of Ledger.Withdrawal_handle.t
[@@deriving yojson]

let empty () =
  {
    ledger = Ledger.empty;
    vm_state = External_vm.External_vm_protocol.State.empty;
  }

let intialize_external_vm_state vm_state t = { t with vm_state }

let ledger t = t.ledger

let vm_state t = t.vm_state

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

let apply_user_operation t operation_hash user_operation =
  let open User_operation in
  let { source; initial_operation; hash = tx_hash } = user_operation in
  let { ledger; vm_state } = t in
  match initial_operation with
  | Transaction { destination; amount; ticket } ->
    let%ok ledger =
      Ledger.transfer ~sender:source ~destination amount ticket ledger in
    Ok ({ ledger; vm_state }, None)
  | Tezos_withdraw { owner; amount; ticket } ->
    let%ok ledger, handle =
      Ledger.withdraw ~sender:source ~destination:owner amount ticket ledger
    in
    Ok ({ ledger; vm_state }, Some (Receipt_tezos_withdraw handle))
  | Vm_transaction { payload } ->
    let vm_state =
      External_vm.External_vm_client.apply_vm_operation ~state:t.vm_state
        ~source ~op_hash:operation_hash ~tx_hash payload in
    Ok ({ ledger; vm_state }, None)

let apply_user_operation t hash user_operation =
  match apply_user_operation t hash user_operation with
  | Ok (t, receipt) -> (t, receipt)
  | Error `Not_enough_funds -> (t, None)
