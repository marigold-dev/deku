module type Request_endpoint = sig
  type request
  val request_to_yojson : request -> Yojson.Safe.t
  val request_of_yojson :
    Yojson.Safe.t -> request Ppx_deriving_yojson_runtime.error_or
  type response
  val response_to_yojson : response -> Yojson.Safe.t
  val response_of_yojson :
    Yojson.Safe.t -> response Ppx_deriving_yojson_runtime.error_or
  val path : string
end
exception Error_status
val broadcast_to_list :
  (module Request_endpoint with type request = 'a) ->
  Uri.t list ->
  'a ->
  unit Lwt.t
val broadcast_to_validators :
  (module Request_endpoint with type request = 'a) ->
  State.t ->
  'a ->
  unit Lwt.t
val request_block_by_hash :
  Network_schemas.Block_by_hash_spec.request ->
  Uri.t ->
  Network_schemas.Block_by_hash_spec.response Lwt.t
val request_block_level :
  unit -> Uri.t -> Network_schemas.Block_level.response Lwt.t
val request_protocol_snapshot :
  unit -> Uri.t -> Network_schemas.Protocol_snapshot.response Lwt.t
val request_nonce :
  Network_schemas.Request_nonce.request ->
  Uri.t ->
  Network_schemas.Request_nonce.response Lwt.t
val request_register_uri :
  Network_schemas.Register_uri.request -> Uri.t -> unit Lwt.t
val request_withdraw_proof :
  Network_schemas.Withdraw_proof.request ->
  Uri.t ->
  Network_schemas.Withdraw_proof.response Lwt.t
val broadcast_signature :
  State.t -> Network_schemas.Signature_spec.request -> unit Lwt.t
val broadcast_block_and_signature :
  State.t -> Network_schemas.Block_and_signature_spec.request -> unit Lwt.t
val broadcast_user_operation_gossip :
  State.t -> Network_schemas.User_operation_gossip.request -> unit Lwt.t
val broadcast_user_operation_gossip_to_list :
  Uri.t list -> Network_schemas.User_operation_gossip.request -> unit Lwt.t
val request_user_operation_gossip :
  Network_schemas.User_operation_gossip.request -> Uri.t -> unit Lwt.t
val request_consensus_operation :
  Network_schemas.Consensus_operation_gossip.request -> Uri.t -> unit Lwt.t
val request_trusted_validator_membership :
  Network_schemas.Trusted_validators_membership_change.request ->
  Uri.t ->
  unit Lwt.t
