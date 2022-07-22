open Helpers
open Crypto
open Tezos

type t = {
  rpc_node : Uri.t;
  secret : Secret.t;
  consensus_contract : Address.t;
  discovery_contract : Address.t;
  required_confirmations : int;
  bridge_process : Tezos_bridge.t;
}

let make ~rpc_node ~secret ~consensus_contract ~discovery_contract
    ~required_confirmations =
  let bridge_process = Tezos_bridge.spawn () in
  {
    rpc_node;
    secret;
    consensus_contract;
    discovery_contract;
    required_confirmations;
    bridge_process;
  }

module Run_contract = struct
  let run t ~destination ~entrypoint ~payload =
    let {
      rpc_node;
      secret;
      required_confirmations;
      consensus_contract = _;
      discovery_contract = _;
      bridge_process;
    } =
      t in
    Tezos_bridge.inject_transaction bridge_process ~rpc_node ~secret
      ~required_confirmations ~destination ~entrypoint ~payload
end

module Fetch_storage : sig
  val run :
    t ->
    rpc_node:Uri.t ->
    required_confirmations:int ->
    contract_address:Address.t ->
    (Michelson.t, string) result Lwt.t
end = struct
  let run t ~rpc_node ~required_confirmations ~contract_address =
    Tezos_bridge.storage t.bridge_process ~rpc_node ~required_confirmations
      ~destination:contract_address
end

module Fetch_big_map_keys : sig
  val run :
    t ->
    rpc_node:Uri.t ->
    required_confirmations:int ->
    contract_address:Address.t ->
    keys:Michelson.big_map_key list ->
    (Yojson.Safe.t option list, string) result Lwt.t
end = struct
  let run t ~rpc_node ~required_confirmations ~contract_address ~keys =
    Tezos_bridge.big_map_keys t.bridge_process ~rpc_node ~required_confirmations
      ~destination:contract_address ~keys
end

module Listen_transactions = struct
  let listen t ~rpc_node ~required_confirmations ~destination ~on_message =
    let message_stream =
      Tezos_bridge.listen_transaction t.bridge_process ~rpc_node
        ~required_confirmations ~destination in
    Lwt.async (fun () -> Lwt_stream.iter on_message message_stream)
end

module Consensus = struct
  open Michelson.Michelson_v1_primitives
  open Tezos_micheline

  let commit_state_hash t ~block_height ~block_payload_hash ~state_hash
      ~withdrawal_handles_hash ~validators ~signatures =
    let module Payload = struct
      type t = {
        block_height : int64;
        block_payload_hash : BLAKE2B.t;
        signatures : string option list;
        handles_hash : BLAKE2B.t;
        state_hash : BLAKE2B.t;
        validators : string list;
        current_validator_keys : string option list;
      }
      [@@deriving to_yojson]
    end in
    let open Payload in
    let current_validator_keys, signatures =
      List.map
        (fun signature ->
          match signature with
          | Some (key, signature) ->
            let key = Key.to_string key in
            let signature = Signature.to_string signature in
            (Some key, Some signature)
          | None -> (None, None))
        signatures
      |> List.split in
    let validators = List.map Key_hash.to_string validators in
    let payload =
      {
        block_height;
        block_payload_hash;
        signatures;
        handles_hash = withdrawal_handles_hash;
        state_hash;
        validators;
        current_validator_keys;
      } in
    (* TODO: check result *)
    let%await _ =
      Run_contract.run t ~destination:t.consensus_contract
        ~entrypoint:"update_root_hash"
        ~payload:(Payload.to_yojson payload) in
    await ()

  type transaction =
    | Deposit          of {
        ticket : Ticket_id.t;
        amount : Z.t;
        destination : Address.t;
      }
    | Update_root_hash of BLAKE2B.t

  type operation = {
    hash : Operation_hash.t;
    transactions : transaction list;
  }

  let parse_transaction transaction =
    let Tezos_bridge.Listen_transaction.{ entrypoint; value } = transaction in
    let value = Micheline.root value in
    match (entrypoint, value) with
    | ( "update_root_hash",
        Tezos_micheline.Micheline.Prim
          ( _,
            D_Pair,
            [
              Prim
                ( _,
                  D_Pair,
                  [
                    Prim
                      ( _,
                        D_Pair,
                        [Bytes (_, _block_hash); Int (_, _block_height)],
                        _ );
                    Prim
                      ( _,
                        D_Pair,
                        [Bytes (_, _block_payload_hash); Int (_, _handles_hash)],
                        _ );
                  ],
                  _ );
              Prim
                ( _,
                  D_Pair,
                  [
                    Prim
                      (_, D_Pair, [_signatures; Bytes (_, state_root_hash)], _);
                    _;
                  ],
                  _ );
            ],
            _ ) ) ->
      let%some state_root_hash =
        state_root_hash |> Bytes.to_string |> BLAKE2B.of_raw_string in
      Some (Update_root_hash state_root_hash)
    | ( "deposit",
        Micheline.Prim
          ( _,
            D_Pair,
            [
              Bytes (_, destination);
              Prim
                ( _,
                  D_Pair,
                  [
                    Bytes (_, ticketer);
                    Prim (_, D_Pair, [Bytes (_, data); Int (_, amount)], _);
                  ],
                  _ );
            ],
            _ ) ) ->
      let%some destination =
        Data_encoding.Binary.of_bytes_opt Address.encoding destination in
      let%some ticketer =
        Data_encoding.Binary.of_bytes_opt Address.encoding ticketer in
      let ticket =
        let open Ticket_id in
        { ticketer; data } in
      Some (Deposit { ticket; destination; amount })
    | _ -> None

  let parse_operation output =
    let Tezos_bridge.Listen_transaction.{ hash; transactions } = output in
    let%some hash = Operation_hash.of_string hash in
    let transactions = List.filter_map parse_transaction transactions in
    Some { hash; transactions }

  let listen_operations t ~on_operation =
    let on_message output =
      match parse_operation output with
      | Some operation -> on_operation operation
      | None -> () in
    Listen_transactions.listen t ~rpc_node:t.rpc_node
      ~required_confirmations:t.required_confirmations
      ~destination:t.consensus_contract ~on_message

  let fetch_uris_from_discovery t validator_key_hashes =
    let {
      rpc_node;
      required_confirmations;
      consensus_contract = _;
      discovery_contract;
      secret = _;
      bridge_process = _;
    } =
      t in
    let micheline_yojson_to_key_hash = function
      | `String uri -> Ok (Uri.of_string uri)
      | _ -> Error "Failed to parse storage micheline expression" in
    let%await micheline_uris =
      Fetch_big_map_keys.run t ~required_confirmations ~rpc_node
        ~contract_address:discovery_contract
        ~keys:
          (List.map
             (fun key_hash -> Michelson.Key_hash key_hash)
             validator_key_hashes) in
    match micheline_uris with
    | Error e -> Lwt.return (Error e)
    | Ok micheline_uris ->
      let uris =
        List.map_ok
          (fun micheline ->
            match micheline with
            | None -> Ok None
            | Some uri ->
              let%ok key_hash = micheline_yojson_to_key_hash uri in
              Ok (Some key_hash))
          micheline_uris in
      let key_hash_uri_pairs =
        Result.map (fun uris -> List.combine validator_key_hashes uris) uris
      in
      Lwt.return key_hash_uri_pairs

  let fetch_validators t =
    let {
      rpc_node;
      required_confirmations;
      consensus_contract;
      discovery_contract;
      secret = _;
      bridge_process = _;
    } =
      t in
    let micheline_to_validators michelson =
      let%ok michelson = michelson in
      let michelson = Micheline.root michelson in
      match michelson with
      | Micheline.Prim
          ( _,
            D_Pair,
            [Prim (_, D_Pair, [_; _; Seq (_, key_hashes)], _); _; _],
            _ ) ->
        List.fold_left_ok
          (fun acc k ->
            match k with
            | Micheline.String (_, k) -> (
              match Key_hash.of_string k with
              | Some k -> Ok (k :: acc)
              | None -> Error ("Failed to parse " ^ k))
            | _ -> Error "Some key_hash wasn't of type string")
          [] (List.rev key_hashes)
      | _ -> Error "Failed to parse storage micheline expression" in
    let%await micheline_storage =
      Fetch_storage.run t ~required_confirmations ~rpc_node
        ~contract_address:consensus_contract in
    let validators = micheline_to_validators micheline_storage in
    match validators with
    | Ok validators -> (
      let%await validator_uri_pairs = fetch_uris_from_discovery t validators in
      match validator_uri_pairs with
      | Error e -> Lwt.return (Error e)
      | Ok validator_uri_pairs ->
        List.iter
          (function
            | key_hash, None ->
              Log.info
                "Validator with key_hash %s not found in discovery contract \
                 (%s)."
                (Key_hash.to_string key_hash)
                (Address.to_string discovery_contract)
            | _, Some _ -> ())
          validator_uri_pairs;
        Lwt.return (Ok validator_uri_pairs))
    | Error e -> Lwt.return (Error e)
end

module Discovery = struct
  open Pack

  let sign secret ~nonce uri =
    to_bytes
      (pair
         (int (Z.of_int64 nonce))
         (bytes (Bytes.of_string (Uri.to_string uri))))
    |> Bytes.to_string
    |> BLAKE2B.hash
    |> Signature.sign secret
end
