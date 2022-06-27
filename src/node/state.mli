open Crypto
open Protocol
open Consensus

type identity = {
  secret : Secret.t;
  key : Key.t;
  t : Key_hash.t;
  uri : Uri.t;
}
[@@deriving yojson]

module Address_map : Map.S with type key = Key_hash.t

module Uri_map : Map.S with type key = Uri.t

type pollinate_context =
  | Client
  | Server

type t = {
  identity : identity;
  consensus : Consensus.t;
  interop_context : Tezos_interop.t;
  data_folder : string;
  applied_blocks : Block.t list;
  uri_state : string Uri_map.t;
  validators_uri : Uri.t Address_map.t;
  recent_operation_receipts : Core_deku.State.receipt BLAKE2B.Map.t;
  persist_trusted_membership_change :
    Trusted_validators_membership_change.t list -> unit Lwt.t;
  pollinate_node : Pollinate.PNode.t ref Lwt.t;
}

val make :
  identity:identity ->
  trusted_validator_membership_change:Trusted_validators_membership_change.Set.t ->
  persist_trusted_membership_change:
    (Trusted_validators_membership_change.t list -> unit Lwt.t) ->
  interop_context:Tezos_interop.t ->
  data_folder:string ->
  initial_validators_uri:Uri.t Address_map.t ->
  pollinate_context:pollinate_context ->
  t
