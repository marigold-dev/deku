open Contracts.Contract_vm
module Map = Map.Make (Contract_address)

type storage = Contract.t Map.t
type t = storage

let empty : Contract.t Map.t = Map.empty

let originate_contract ~address ~contract storage =
  Map.add address contract storage

let get_contract storage ~address = Map.find_opt address storage

let update_contract_storage ~address ~updated_contract storage =
  Map.update address (Option.map (fun _ -> updated_contract)) storage
