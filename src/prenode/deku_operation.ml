(* open Bin_prot.Std
 *)
type consensus_operation =
  | Proposal
  | Prevote
  | Precommit
[@@deriving eq, ord, bin_io, show { with_path = false }]

type block_operation =
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
  | Block
[@@deriving eq, ord, bin_io, show { with_path = false }]

type operation_detail =
  | Consensus_operation
  | Consensus_governance_operation
  | Block_operation
[@@deriving eq, ord, bin_io, show { with_path = false }]

type t = {
  operation_family : operation_family;
  operation_detail : operation_detail;
}
[@@deriving eq, ord, bin_io, show { with_path = false }]

let get_operation_family t = t.operation_family

let get_operatil_detail t = t.operation_detail
