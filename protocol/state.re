open Helpers;

module Operation_side_chain_set = Set_with_yojson_make(Operation.Side_chain);
[@deriving yojson]
type t = {
  ledger: Ledger.t,
  // TODO: more efficient lookup on included_operations
  // TODO: is this part of the protocol?
  included_operations: Operation_side_chain_set.t,
  validators: Validators.t,
  block_height: int64,
  last_block_hash: string,
  state_root_hash: string,
};

