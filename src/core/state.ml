open Helpers
open Crypto
type t = { ledger : Ledger.t } [@@deriving yojson]
type receipt = Receipt_tezos_withdraw of Ledger.Handle.t [@@deriving yojson]
let empty = { ledger = Ledger.empty }
let ledger t = t.ledger
let hash t = to_yojson t |> Yojson.Safe.to_string |> BLAKE2B.hash
let apply_tezos_operation t tezos_operation =
  let open Tezos_operation in
  let apply_internal_operation t internal_operation =
    let { ledger } = t in
    match internal_operation with
    | Tezos_deposit { destination; amount; ticket } ->
      let ledger =
        match destination with
        | Implicit key_hash ->
          let destination = key_hash in
          Ledger.deposit destination amount ticket ledger
        | Originated _ -> failwith "not implemented" in
      { ledger } in
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
    Ok ({ ledger }, None)
  | None, Transaction _ -> Error `Transaction_sender_must_be_implicit_account
  | Some sender, Tezos_withdraw { owner; amount; ticket } ->
    let%ok ledger, handle =
      Ledger.withdraw ~sender ~destination:owner amount ticket t.ledger in
    Ok ({ ledger }, Some (Receipt_tezos_withdraw handle))
  | None, Tezos_withdraw _ -> Error `Withdraw_sender_must_be_implicit_account
let apply_user_operation t user_operation =
  match apply_user_operation t user_operation with
  | Ok (t, receipt) -> (t, receipt)
  | Error
      ( `Not_enough_funds | `Transaction_sender_must_be_implicit_account
      | `Withdraw_sender_must_be_implicit_account ) ->
    (t, None)
