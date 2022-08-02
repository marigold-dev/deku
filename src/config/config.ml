open Deku_stdlib
open Deku_crypto

exception Invalid_json of string

type tezos_config_static = { rpc_node : string; required_confirmations : int }
[@@deriving yojson]

type tezos_config = {
  rpc_node : Uri.t;
  required_confirmations : int;
  (* consensus_contract_address : Tezos.Address.t; *)
  (* discovery_contract_address : Tezos.Address.t; *)
  tezos_secret_key : Secret.t;
}

type config_static = {
  tezos_config : tezos_config_static;
  bootstrapper_key : string;
  minimum_block_delay : float;
      (* trusted_membership_changes : Trusted_validators_membership_change.t; *)
}
[@@deriving yojson]

type config = {
  tezos_config : tezos_config;
  bootstrapper_key : string;
  minimum_block_delay : float;
  (* trusted_membership_changes : Trusted_validators_membership_change.t; *)
  data_folder : string; (* identity : Consensus.identity; *)
}

let read_json of_yojson ~file =
  let%await string = Lwt_io.with_file ~mode:Input file Lwt_io.read in
  let json = Yojson.Safe.from_string string in
  Lwt.return (of_yojson json)

let read_config ~data_folder ~consensus_contract_address
    ~discovery_contract_address ~tezos_secret_key ~uri ~deku_secret_key file =
  ignore
    ( consensus_contract_address,
      discovery_contract_address,
      uri,
      deku_secret_key );
  (* let identity = Consensus.make_identity ~secret:deku_secret_key ~uri in *)
  let%await {
          tezos_config;
          bootstrapper_key;
          minimum_block_delay;
        (* trusted_membership_changes; *)
        } =
    read_json ~file config_static_of_yojson
  in
  let tezos_config =
    {
      rpc_node = Uri.of_string tezos_config.rpc_node;
      required_confirmations = tezos_config.required_confirmations;
      (* consensus_contract_address; *)
      (* discovery_contract_address; *)
      tezos_secret_key;
    }
  in
  Lwt.return
    {
      tezos_config;
      bootstrapper_key;
      minimum_block_delay;
      (* trusted_membership_changes; *)
      data_folder;
      (* identity; *)
    }
