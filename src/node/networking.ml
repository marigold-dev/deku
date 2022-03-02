open Helpers
open State
open Protocol
module type Request_endpoint = sig
  type request [@@deriving yojson]
  type response [@@deriving yojson]
  val path : string
end
exception Error_status
let request request_to_yojson path data uri =
  let open Piaf in
  let uri = Uri.with_path uri path in
  let body = request_to_yojson data |> Yojson.Safe.to_string |> Body.of_string in
  let%await response = Client.Oneshot.post ~body uri in
  match response with
  | Ok response -> (
    let%await body = Piaf.Body.to_string response.body in
    match body with
    | Ok body -> await body
    | Error _err -> Lwt.fail Error_status)
  | Error _err -> Lwt.fail Error_status
let post (type req) (module E : Request_endpoint with type request = req) data
    uri =
  let%await _body = request E.request_to_yojson E.path data uri in
  await ()
let request (type req res)
    (module E : Request_endpoint with type request = req and type response = res)
    data uri =
  let%await body = request E.request_to_yojson E.path data uri in
  let response =
    Yojson.Safe.from_string body |> E.response_of_yojson |> Result.get_ok in
  await response
let broadcast_to_list endpoint uris data =
  uris
  |> Lwt_list.iter_s (fun uri ->
         Lwt.catch (fun () -> post endpoint data uri) (fun _exn -> await ()))
let broadcast_to_validators endpoint state data =
  Validators.to_list state.protocol.validators
  |> List.filter_map (fun Validators.{ address; _ } ->
         Address_map.find_opt address state.validators_uri)
  |> fun uris -> broadcast_to_list endpoint uris data
open Network_schemas
let request_block_by_hash = request (module Block_by_hash_spec)
let request_block_level = request (module Block_level)
let request_protocol_snapshot = request (module Protocol_snapshot)
let request_nonce = request (module Request_nonce)
let request_register_uri = request (module Register_uri)
let request_withdraw_proof = request (module Withdraw_proof)
let broadcast_signature = broadcast_to_validators (module Signature_spec)
let broadcast_block_and_signature =
  broadcast_to_validators (module Block_and_signature_spec)
let broadcast_user_operation_gossip =
  broadcast_to_validators (module User_operation_gossip)
let broadcast_user_operation_gossip_to_list =
  broadcast_to_list (module User_operation_gossip)
let request_user_operation_gossip = request (module User_operation_gossip)
let request_consensus_operation = request (module Consensus_operation_gossip)
let request_trusted_validator_membership =
  request (module Trusted_validators_membership_change)
