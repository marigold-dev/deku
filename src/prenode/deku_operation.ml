(* open Bin_prot.Std
 *)
type consensus_operation =
  | Proposal
  | Prevote
  | Precommit
[@@deriving eq, ord, bin_io, show { with_path = false }]

type block_pool_operation =
  | Block_by_hash
  | Block_level
[@@deriving eq, ord, bin_io, show { with_path = false }]

type validators_operation =
  | Add_validator
  | Remove_validator
[@@deriving eq, ord, bin_io, show { with_path = false }]

type operation_family =
  | Consensus
  | Validators
  | Block_pool
  | Proxy (* Things to be sent to the proxy for outside world *)
[@@deriving eq, ord, bin_io, show { with_path = false }]

type operation_detail =
  | Consensus_operation
  | Validators_operation of validators_operation
  | Block_operation
  | Block_pool_operation
[@@deriving eq, ord, bin_io, show { with_path = false }]

type t = {
  operation_family : operation_family;
  operation_detail : operation_detail;
}
[@@deriving eq, ord, bin_io, show { with_path = false }]

let family_of_string : string -> operation_family =
 fun family_str ->
  match family_str with
  | "Consensus" -> Consensus
  | "Validators" -> Validators
  | "Block_pool" -> Block_pool
  | "Proxy" -> Proxy
  | _ -> failwith "unknwon operation family"

let detail_of_string : string -> operation_detail =
 fun detail_str ->
  match detail_str with
  | "Consensus_operation" -> Consensus_operation
  | "Add_validator" -> Validators_operation Add_validator
  | "Remove_validator" -> Validators_operation Remove_validator
  | "Block_operation" -> Block_operation
  | "Block_pool_operation" -> Block_pool_operation
  | _ -> failwith "unknwon operation detail"

let make :
    operation_family:operation_family -> operation_detail:operation_detail -> t
    =
 fun ~operation_family ~operation_detail ->
  { operation_family; operation_detail }
let get_operation_family t = t.operation_family

let get_operatil_detail t = t.operation_detail
