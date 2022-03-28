open Helpers
open Crypto
type t = {
  ledger : Ledger.t;
  contract_storage : Smart_contracts.Contract_storage.t;
}
[@@deriving yojson]
type receipt = Receipt_tezos_withdraw of Ledger.Withdrawal_handle.t
[@@deriving yojson]
let empty =
  {
    ledger = Ledger.empty;
    contract_storage = Smart_contracts.Contract_storage.empty;
  }
let ledger t = t.ledger
let contract_storage t = t.contract_storage
let hash t = to_yojson t |> Yojson.Safe.to_string |> BLAKE2B.hash
let apply_tezos_operation t tezos_operation =
  let open Tezos_operation in
  let apply_internal_operation t internal_operation =
    let { ledger; contract_storage = _ } = t in
    match internal_operation with
    | Tezos_deposit { destination; amount; ticket } ->
      let ledger =
        match destination with
        | Implicit key_hash ->
          let destination = key_hash in
          Ledger.deposit (Address.of_key_hash destination) amount ticket ledger
        | Originated _ -> failwith "not implemented" in
      { t with ledger } in
  let { hash = _; payload } = tezos_operation in
  let { tezos_operation_hash = _; internal_operations } = payload in
  List.fold_left apply_internal_operation t internal_operations

let apply_user_operation t user_operation =
  let open User_operation in
  let { hash; sender; initial_operation } = user_operation in
  match (sender, initial_operation) with
  | sender, Transaction { destination; amount; ticket } ->
    let%ok ledger =
      Ledger.transfer ~sender ~destination amount ticket t.ledger in
    Ok ({ t with ledger }, None)
  | sender, Tezos_withdraw { owner; amount; ticket }
    when Address.is_implicit sender ->
    let%ok ledger, handle =
      Ledger.withdraw ~sender ~destination:owner amount ticket t.ledger in
    Ok ({ t with ledger }, Some (Receipt_tezos_withdraw handle))
  | _, Tezos_withdraw _ -> Error `Withdraw_sender_must_be_implicit_account
  | sender, Contract_origination { to_originate; ticket; amount }
    when Address.is_implicit sender ->
    (* @TODO: deduct gas from account and check *)
    let balance = Ledger.balance sender ticket t.ledger in
    let origination_cost = 250 |> Amount.of_int in
    let%assert () =
      Amount.
        ( `Origination_error
            {
              t with
              ledger = Ledger.burn t.ledger ~sender ~ticket ~amount:balance;
            },
          let comparison_result = compare balance origination_cost in
          comparison_result >= 0 ) in
    let%assert () =
      Amount.
        ( `Origination_error
            {
              t with
              ledger = Ledger.burn t.ledger ~sender ~ticket ~amount:balance;
            },
          let comparison_result = compare (balance - origination_cost) amount in
          comparison_result >= 0 ) in
    let t =
      {
        t with
        ledger = Ledger.burn t.ledger ~sender ~ticket ~amount:origination_cost;
      } in
    let initial_gas = Amount.to_int Amount.(balance - origination_cost) in
    let burn_and_update t _ to_burn =
      `Origination_error
        {
          t with
          ledger =
            Ledger.burn ~sender ~ticket ~amount:(to_burn |> Amount.of_int)
              t.ledger;
        } in
    let%ok contract, to_burn =
      Smart_contracts.Contract.Compile.compile_script
        ~originated_by:(Address.to_key_hash sender |> Option.get)
        ~gas:initial_gas ~on_error:(burn_and_update t) to_originate in
    let address = hash |> BLAKE2B.to_raw_string |> BLAKE2B_20.hash in
    let contract_storage =
      Smart_contracts.Contract_storage.originate_contract t.contract_storage
        ~address ~contract in
    let ledger =
      Ledger.burn t.ledger ~sender ~ticket ~amount:Amount.(of_int to_burn) in
    Ledger.transfer ~sender
      ~destination:(Address.of_contract_hash address)
      amount ticket ledger
    |> Result.map_error (fun _ -> `Origination_error { t with ledger })
    |> Result.map (fun ledger -> ({ contract_storage; ledger }, None))
  | _, Contract_origination _ ->
    Error `Contract_originator_must_be_implicit_account
let apply_user_operation t user_operation =
  match apply_user_operation t user_operation with
  | Ok (t, receipt) -> (t, receipt)
  | Error (`Origination_error t) -> (t, None)
  | Error
      ( `Not_enough_funds | `Withdraw_sender_must_be_implicit_account
      | `Contract_originator_must_be_implicit_account ) ->
    (t, None)
