open Crypto
open Protocol
open Consensus
module Address_map = Map.Make (Key_hash)
module Uri_map = Map.Make (Uri)

type t = {
  config : Config.t;
  consensus : Consensus.t;
  interop_context : Tezos_interop.t;
  data_folder : string;
  (* TODO: we need a bound on the size of this and put
     behind an abstract type. We should also change how
     this works once we have an indexer. See https://github.com/marigold-dev/deku/issues/535 *)
  applied_blocks : Block.t list;
  uri_state : string Uri_map.t;
  validators_uri : Uri.t Address_map.t;
  recent_operation_receipts : Core_deku.State.receipt BLAKE2B.Map.t;
  persist_trusted_membership_change :
    Trusted_validators_membership_change.t list -> unit Lwt.t;
}

let make ~config ~trusted_validator_membership_change
    ~persist_trusted_membership_change ~interop_context ~data_folder
    ~initial_validators_uri =
  let consensus =
    Consensus.make ~identity:config.Config.identity
      ~trusted_validator_membership_change in

  {
    config;
    consensus;
    interop_context;
    data_folder;
    applied_blocks = [];
    uri_state = Uri_map.empty;
    validators_uri = initial_validators_uri;
    recent_operation_receipts = BLAKE2B.Map.empty;
    persist_trusted_membership_change;
  }
