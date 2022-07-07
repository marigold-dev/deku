open Helpers
open Crypto
open Protocol
open Consensus
module Node = State

let find_random_validator_uri state =
  let random_int v = v |> Int32.of_int |> Random.int32 |> Int32.to_int in
  let validators = Validators.to_list state.Node.consensus.protocol.validators in
  let rec safe_validator_uri () =
    let validator = List.nth validators (random_int (List.length validators)) in
    if state.Node.identity.t = validator.address then
      safe_validator_uri ()
    else
      match
        Node.Address_map.find_opt validator.address state.validators_uri
      with
      | Some uri -> uri
      | None -> safe_validator_uri () in
  safe_validator_uri ()

let validator_uris state =
  let validators = Validators.to_list state.Node.consensus.protocol.validators in
  List.filter_map
    (fun Validators.{ address; _ } ->
      Node.Address_map.find_opt address state.Node.validators_uri)
    validators

let uri_to_pollinate : Uri.t -> Pollinate.Address.t =
 fun uri ->
  Log.debug "Translating Uri.t to Pollinate.Address.t";
  let address =
    match Uri.host uri with
    | Some "localhost" -> "127.0.0.1"
    | Some "0.0.0.0" -> "127.0.0.1"
    | Some address -> address
    | _ -> failwith "Could not retrieve address from uri" in
  let port =
    match Uri.port uri with
    | Some port -> port + 100 (* ugly fix to avoif using the HTTP port *)
    | None -> failwith "Could not retrieve port from uri." in
  Pollinate.Address.create address port

let broadcast_signature state ~hash ~signature =
  Lwt.async (fun () ->
      let%await node = state.Node.pollinate_node in
      let recipients = List.map uri_to_pollinate (validator_uris state) in
      Network.broadcast_signature node { hash; signature } recipients)

let broadcast_block state ~block =
  Lwt.async (fun () ->
      let%await node = state.Node.pollinate_node in
      let%await () = Lwt_unix.sleep 1.0 in
      let recipients = List.map uri_to_pollinate (validator_uris state) in
      Network.broadcast_block node { block } recipients)

let broadcast_user_operation_gossip state operation =
  let uris = validator_uris state in
  Network.broadcast_user_operation_gossip uris operation
