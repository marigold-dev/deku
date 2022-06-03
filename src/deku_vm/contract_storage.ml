open Helpers
open Contract_vm
open External_vm.External_vm_server

let initial_state = [] (* TODO: Should be of type State.t and not set list*)

let originate_contract storage ~address ~contract =
  storage.set
    (address |> Contract_address.to_string)
    (contract |> Contract.to_yojson)

let update_contract_storage storage ~address ~updated_contract =
  let address = address |> Contract_address.to_string in
  let updated_contract = updated_contract |> Contract.to_yojson in
  match storage.get address with
  | None -> ()
  | Some _ -> storage.set address updated_contract

let get_contract storage ~address =
  let address = address |> Contract_address.to_string in
  storage.get address
  |> Option.map Contract.of_yojson
  |> Option.map Result.to_option
  |> Option.join
