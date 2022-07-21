open Crypto
open Protocol
open Helpers
module Operation_map = Map.Make (Operation)

type identity = {
  secret : Secret.t;
  key : Key.t;
  t : Key_hash.t;
  uri : Uri.t;
}
[@@deriving yojson]

let make_identity ~secret ~key ~uri =
  { secret; key; t = Key_hash.of_key key; uri }

type state = {
  (* TODO: duplicated on Node.State.t *)
  identity : identity;
  trusted_validator_membership_change :
    Trusted_validators_membership_change.Set.t;
  pending_operations : float Operation_map.t;
  block_pool : Block_pool.t;
  protocol : Protocol.t;
  snapshots : Snapshots.t;
}

type t = state

let make ~identity ~trusted_validator_membership_change =
  let initial_block = Block.genesis in
  let initial_protocol = Protocol.make ~initial_block in
  let initial_signatures =
    Signatures.make ~self_key:identity.key |> Signatures.set_signed in
  let initial_block_pool =
    Block_pool.make ~self_key:identity.key
    |> Block_pool.append_block initial_block in
  let hash, data = Protocol.hash initial_protocol in
  let initial_snapshot =
    let open Snapshots in
    { hash; data } in
  let initial_snapshots =
    Snapshots.make ~initial_snapshot ~initial_block ~initial_signatures in
  {
    identity;
    trusted_validator_membership_change;
    pending_operations = Operation_map.empty;
    block_pool = initial_block_pool;
    protocol = initial_protocol;
    snapshots = initial_snapshots;
  }

let is_next block state = Protocol.is_next state.protocol block

let is_current_producer state ~key_hash =
  match get_current_block_producer state.protocol with
  | Some current_producer -> current_producer.address = key_hash
  | None -> false

let minimum_signable_time_between_epochs = 10.0

let maximum_signable_time_between_epochs = 20.0

(** Used to add a delay between a tezos operation being confirmed,
  needs to be bigger than the polling interval for operations *)
let minimum_waiting_period_for_tezos_operation = 5.0

(* TODO: this is not sound - nodes should be out of sync when they
   load a snapshot until they verify the next state root hash on Tezos. *)
let in_sync state =
  (* When a node is on the genesis block, it is by definition not in sync.
     However, state.protocol.last_applied_block_timestamp is set to Unix.time ()
     when the chain initializes, so we need this extra check here. *)
  if BLAKE2B.equal Block.genesis.hash state.protocol.last_block_hash then
    false
  else
    let validators =
      Validators.length state.protocol.validators |> Float.of_int in
    let time_since_last_applied_block =
      Unix.time () -. state.protocol.last_applied_block_timestamp in
    time_since_last_applied_block
    <= validators *. Protocol.block_producer_timeout
