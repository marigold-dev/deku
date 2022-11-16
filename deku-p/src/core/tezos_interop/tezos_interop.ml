open Deku_stdlib
open Deku_crypto
open Deku_tezos

type interop = Tezos_bridge.t
and t = interop

type transaction =
  | Deposit of { ticket : Ticket_id.t; amount : Z.t; destination : Key_hash.t }
  | Update_root_hash of BLAKE2b.t

type operation = {
  hash : Tezos_operation_hash.t;
  transactions : transaction list;
}

let parse_transaction transaction =
  let open Michelson.Michelson_v1_primitives in
  let open Tezos_micheline in
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
                      [ Bytes (_, _block_hash); Int (_, _block_height) ],
                      _ );
                  Prim
                    ( _,
                      D_Pair,
                      [ Bytes (_, _block_payload_hash); Int (_, _handles_hash) ],
                      _ );
                ],
                _ );
            Prim
              ( _,
                D_Pair,
                [
                  Prim
                    (_, D_Pair, [ _signatures; Bytes (_, state_root_hash) ], _);
                  _;
                ],
                _ );
          ],
          _ ) ) ->
      let%some state_root_hash =
        state_root_hash |> Bytes.to_string |> BLAKE2b.of_raw
      in
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
                  Prim (_, D_Pair, [ Bytes (_, data); Int (_, amount) ], _);
                ],
                _ );
          ],
          _ ) ) ->
      let%some destination =
        Data_encoding.Binary.of_bytes_opt Key_hash.encoding destination
      in
      let%some ticketer =
        Data_encoding.Binary.of_bytes_opt Address.encoding ticketer
      in
      let ticket =
        let open Ticket_id in
        { ticketer; data }
      in
      Some (Deposit { ticket; destination; amount })
  | _ -> None

let parse_operation output =
  let Tezos_bridge.Listen_transaction.{ hash; transactions } = output in
  let%some hash = Tezos_operation_hash.of_b58 hash in
  let transactions = List.filter_map parse_transaction transactions in
  Some { hash; transactions }

let start ~sw ~rpc_node ~secret ~consensus_contract ~on_operation =
  let on_transactions ~transactions =
    match parse_operation transactions with
    | Some operation -> on_operation operation
    | None -> ()
  in
  Tezos_bridge.spawn ~sw ~rpc_node ~secret ~destination:consensus_contract
    ~on_transactions

let commit_state_hash interop ~block_level ~block_payload_hash ~state_hash
    ~withdrawal_handles_hash ~validators ~signatures =
  let block_level =
    Deku_concepts.Level.to_n block_level |> N.to_z |> Z.to_int64
  in
  let module Payload = struct
    type t = {
      block_level : int64;
      block_payload_hash : BLAKE2b.t;
      signatures : string option list;
      handles_hash : BLAKE2b.t;
      state_hash : BLAKE2b.t;
      validators : string list;
      current_validator_keys : string option list;
    }
    [@@deriving yojson]
  end in
  let open Payload in
  let current_validator_keys, signatures =
    List.map
      (fun signature ->
        match signature with
        | Some (key, signature) ->
            let key = Key.to_b58 key in
            let signature = Signature.to_b58 signature in
            (Some key, Some signature)
        | None -> (None, None))
      signatures
    |> List.split
  in
  let validators = List.map Key_hash.to_b58 validators in
  let payload =
    {
      block_level;
      block_payload_hash;
      signatures;
      handles_hash = withdrawal_handles_hash;
      state_hash;
      validators;
      current_validator_keys;
    }
  in
  (* TODO: check result *)
  let transaction : Tezos_bridge.Inject_transaction.t option =
    Tezos_bridge.inject_transaction interop ~entrypoint:"update_root_hash"
      ~payload:(Payload.yojson_of_t payload)
  in

  let open Tezos_bridge.Inject_transaction in
  match transaction with
  | Some (Applied { hash }) ->
      Logs.debug (fun m ->
          m "Successfully committed state hash on operation %s" hash)
  | Some (Failed _) -> Logs.warn (fun m -> m "Failed to commit state hash")
  | Some (Skipped _) -> Logs.warn (fun m -> m "Tezos operation was skipped")
  | Some (Backtracked _) ->
      Logs.warn (fun m -> m "Tezos operation was backtracked")
  | Some (Unknown _) ->
      Logs.warn (fun m -> m "Unknown result of Tezos operation")
  | Some (Error { error }) -> (
      match error with
      | Unknown err ->
          Logs.err (fun m -> m "Received error from Tezos Bridge: %s" err)
      | Insufficient_balance address ->
          Logs.err (fun m -> m "Insufficient tez balance for %s" address)
      | Consensus_contract error ->
          Logs.err (fun m -> m "Consensus smart contract error: %s" error)
      | Several_operations error ->
          Logs.warn (fun m ->
              m "Submitting several operations in the same block: %s" error))
  | None ->
      (* TODO: I think we can improve the types of this - maybe result would be better? *)
      Logs.warn (fun m ->
          m "Tezos bridge had an exception while trying to commit state hash")
