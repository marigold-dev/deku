open Helpers
open Crypto
open Tezos
module Context = struct
  type t = {
    rpc_node : Uri.t;
    secret : Secret.t;
    consensus_contract : Address.t;
    required_confirmations : int;
  }
end
module Consensus = struct
  let commit_state_hash ~context ~block_height ~block_payload_hash ~state_hash
      ~withdrawal_handles_hash ~validators ~signatures =
    Lwt.return_unit

  type transaction =
    | Deposit          of {
        ticket : Ticket_id.t;
        amount : Z.t;
        destination : Address.t;
      }
    | Update_root_hash of BLAKE2B.t
  type operation = {
    hash : Operation_hash.t;
    transactions : transaction list;
  }
  let listen_operations ~context ~on_operation = ()
  let fetch_validators ~context = Lwt.return_ok []
end [@warning "-27"]
