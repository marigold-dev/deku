open Helpers
open Bin_common
open Cmdliner

(*************************************************************************)
(* making Deku accounts - wallet *)

type wallet = {
  key_hash : Crypto.Key_hash.t;
  secret : Crypto.Secret.t;
}

let make_wallet key_hash secret =
  {
    key_hash = Crypto.Key_hash.of_string key_hash |> Option.get;
    secret = Crypto.Secret.of_string secret |> Option.get;
  }

let alice_wallet =
  make_wallet "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf"
    "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"

let bob_wallet =
  make_wallet "tz1h1oFuYsCorjxekQ59bUe1uDGhuYvEx9ob"
    "edsk326F1xfCvHFw1LWhgtrwcm6DnFoHCmjjWX4vcWsJCbqmujJQVs"

(*************************************************************************)
(* Validators uris *)

(*let get_random_validator_uri validators_uris () =
  (* TODO: use Random *)
  List.nth validators_uris 0 |> Uri.of_string*)

(*let get_current_block_level node_folder () =
  let open Network in
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris -> (
    let validator_uris = List.filter_map (function
    | key_hash, Some uri -> Some (key_hash, uri)
    | _ -> None)
    validator_uris
    in
    match validator_uris with
    | [] -> Lwt.return (`Error (false, "No validators found"))
    | (_, validator_uri) :: _ ->
     let%await block_level = Network.request_block_level () validator_uri in
     let block_level = block_level.level in
     Lwt.return block_level
    ) *)

open Pipeline

let interop_context =
  let node_folder = read_file "./data/tezos.json" in
  let%await context =
    Files.Interop_context.read ~file:(node_folder ^ "/tezos.json") in
  Lwt.return
    (Tezos_interop.make ~rpc_node:context.rpc_node ~secret:context.secret
       ~consensus_contract:context.consensus_contract
       ~discovery_contract:context.discovery_contract
       ~required_confirmations:context.required_confirmations)

let validator_uris ~interop_context =
  Tezos_interop.Consensus.fetch_validators interop_context

(*let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]*)

let validator_uris =
  let open Network in
  let%await interop_context = interop_context in
  let%await validators_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris ->
    List.filter_map
      (function
        | key_hash, Some uri -> Some (key_hash, uri)
        | _ -> None)
      validator_uris

let get_random_validator_uri () =
  List.nth validators_uris (Random.int 0) |> Uri.of_string

(*************************************************************************)
(* Current block level *)

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = Network.request_block_level () validator_uri in
  Lwt.return block_level.level

(*************************************************************************)
(* Ticket *)

let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated { contract; entrypoint = None } in
  Core.Ticket_id.{ ticketer; data = Bytes.empty }
