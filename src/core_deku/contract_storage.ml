open Helpers
open Contracts.Contract_vm
module Map = Map.Make_with_yojson (Contract_address)

type t = Contract.t Map.t [@@deriving yojson, eq]

let empty = Map.empty

let originate_contract t ~address ~contract = Map.add address contract t

let update_contract_storage t ~address ~updated_contract =
  Map.update address (Option.map (fun _ -> updated_contract)) t

let get_contract t ~address = Map.find_opt address t
