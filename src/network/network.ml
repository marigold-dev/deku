include Network_client
include Network_packet

let request_block_by_hash = request (module Block_by_hash_spec)

let request_block_level = request (module Block_level)

let request_protocol_snapshot = request (module Protocol_snapshot)

let request_nonce = request (module Request_nonce)

let request_register_uri = request (module Register_uri)

let request_withdraw_proof = request (module Withdraw_proof)

let broadcast_signature = send_over_pollinate (module Signature_spec)

let broadcast_block = send_over_pollinate (module Block_spec)

let broadcast_user_operation_gossip =
  broadcast_to_list (module User_operation_gossip)

let broadcast_user_operation_gossip_to_list =
  broadcast_to_list (module User_operation_gossip)

let request_user_operation_gossip = request (module User_operation_gossip)

let request_user_operations_gossip = request (module User_operations_gossip)

let request_consensus_operation = request (module Consensus_operation_gossip)

let request_trusted_validator_membership =
  request (module Trusted_validators_membership_change)

let broadcast_validators_change = send_over_pollinate (module Validators_change)

let request_ticket_balance = request (module Ticket_balance)
