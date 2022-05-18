(* open Bin_prot.Std
 *)
type consensus_operation =
  | Proposal
  | Prevote
  | Precommit
[@@deriving bin_io, show { with_path = false }]

type block_operation =
  | Block_by_hash
  | Block_level
[@@deriving bin_io, show { with_path = false }]

type consensus_governance_operation =
  | Add_validator
  | Remove_validator
[@@deriving bin_io, show { with_path = false }]

type operation_family =
  | Consensus
  | Consensus_governance
  | Block
[@@deriving bin_io, show { with_path = false }]

type operation_detail =
  | Consensus_operation
  | Consensus_governance_operation
  | Block_operation
[@@deriving bin_io, show { with_path = false }]

type t = {
  operation_family : operation_family;
  operation_detail : operation_detail;
}
[@@deriving bin_io, show { with_path = false }]
