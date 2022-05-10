type t = {
  category : string; (* Consensus, Operation, ConsensusGovernance, ... *)
  subcategory : string; (* Proposal, Precommit, Withdraw, AddValidator, ... *)
  payload : bytes;
  sender : Crypto.Key_hash.t;
  recipient : Crypto.Key_hash.t option;
}
