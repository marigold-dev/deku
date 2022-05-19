open Helpers

(* hardcode currently
   - data/0/identity.json: uri = "http://localhost:4440"
   and so on
*)
let validators_uris =
  [
    "http://localhost:4440";
    "http://localhost:4441";
    "http://localhost:4442";
    "http://localhost:4443";
  ]

let get_random_validator_uri () =
  let random_int v = v |> Int32.of_int |> Random.int32 |> Int32.to_int in
  let validator = List.nth validators_uris (random_int (List.length validators_uris)) in
  Format.eprintf "Validator: %s\n%!" validator;
  validator |> Uri.of_string

(********************************************************************)
(* TODO: get a list of validators uris in indentity.json in each folder
   node_folder: data/subfolder# *)
(*
let interop_context node_folder =
  let%await context =
    Files.Interop_context.read ~file:(node_folder ^ "/tezos.json") in
  Lwt.return
    (Tezos_interop.make ~rpc_node:context.rpc_node ~secret:context.secret
       ~consensus_contract:context.consensus_contract
       ~discovery_contract:context.discovery_contract
       ~required_confirmations:context.required_confirmations)

let validator_uris ~interop_context =
  Tezos_interop.Consensus.fetch_validators interop_context

let get_validators node_folder =
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris ->
    let validator_uris =
      List.filter_map
        (function
          | key_hash, Some uri -> Some (key_hash, uri)
          | _ -> None)
        validator_uris in
    Lwt.return (`Ok validator_uris)*)
