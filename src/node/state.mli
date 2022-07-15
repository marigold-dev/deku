open Crypto
open Protocol
open Consensus

module Address_map : Map.S with type key = Key_hash.t

module Uri_map : Map.S with type key = Uri.t

type pollinate_context =
  | Client
  | Server

type timestamp = float

type t = {
  config : Config.t;
  consensus : Consensus.t;
  interop_context : Tezos_interop.t;
  data_folder : string;
  applied_blocks : (timestamp * Block.t) list;
  uri_state : string Uri_map.t;
  validators_uri : Uri.t Address_map.t;
  recent_operation_receipts : Core_deku.State.receipt BLAKE2B.Map.t;
  persist_trusted_membership_change :
    Trusted_validators_membership_change.t list -> unit Lwt.t;
  pollinate_node : Pollinate.PNode.t ref Lwt.t;
}

val make :
  config:Config.t ->
  trusted_validator_membership_change:Trusted_validators_membership_change.Set.t ->
  persist_trusted_membership_change:
    (Trusted_validators_membership_change.t list -> unit Lwt.t) ->
  interop_context:Tezos_interop.t ->
  data_folder:string ->
  initial_validators_uri:Uri.t Address_map.t ->
  pollinate_context:pollinate_context ->
  t
