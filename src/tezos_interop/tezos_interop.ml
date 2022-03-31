open Helpers
open Crypto
open Tezos
module Context = struct
  type t = {
    rpc_node : Uri.t;
    secret : Secret.t;
    consensus_contract : Address.t;
    required_confirmations : int;
  }
end
module Run_contract = struct
  type input = {
    rpc_node : string;
    secret : string;
    confirmation : int;
    destination : string;
    entrypoint : string;
    payload : Yojson.Safe.t;
  }
  [@@deriving to_yojson]
  type output =
    | Applied     of { hash : string }
    | Failed      of { hash : string }
    | Skipped     of { hash : string }
    | Backtracked of { hash : string }
    | Unknown     of { hash : string }
    | Error       of string
  let output_of_yojson json =
    let module T = struct
      type t = { status : string } [@@deriving of_yojson { strict = false }]

      and finished = { hash : string }

      and error = { error : string }
    end in
    let finished make =
      let%ok { hash } = T.finished_of_yojson json in
      Ok (make hash) in
    let%ok { status } = T.of_yojson json in
    match status with
    | "applied" -> finished (fun hash -> Applied { hash })
    | "failed" -> finished (fun hash -> Failed { hash })
    | "skipped" -> finished (fun hash -> Skipped { hash })
    | "backtracked" -> finished (fun hash -> Backtracked { hash })
    | "unknown" -> finished (fun hash -> Unknown { hash })
    | "error" ->
      let%ok { error } = T.error_of_yojson json in
      Ok (Error error)
    | _ -> Error "invalid status"
  let file = Scripts.file_run_entrypoint
  let run ~context ~destination ~entrypoint ~payload =
    let input =
      {
        rpc_node = context.Context.rpc_node |> Uri.to_string;
        secret = context.secret |> Secret.to_string;
        confirmation = context.required_confirmations;
        destination = Address.to_string destination;
        entrypoint;
        payload;
      } in
    let command = "node" in
    let%await output =
      Lwt_process.pmap
        (command, [|command; file|])
        (Yojson.Safe.to_string (input_to_yojson input)) in
    match Yojson.Safe.from_string output |> output_of_yojson with
    | Ok data ->
      Format.eprintf "Commit operation result: %s\n%!" output;
      await data
    | Error error -> await (Error error)
end
let michelson_of_yojson json =
  let%ok json = Yojson.Safe.to_string json |> Data_encoding.Json.from_string in
  try
    Ok
      (Tezos_micheline.Micheline.root
         (Data_encoding.Json.destruct Michelson.expr_encoding json))
  with
  | _ -> Error "invalid json"
type michelson =
  (int, Michelson.Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node
module Fetch_storage : sig
  val run :
    rpc_node:Uri.t ->
    confirmation:int ->
    contract_address:Address.t ->
    (michelson, string) result Lwt.t
end = struct
  type input = {
    rpc_node : string;
    confirmation : int;
    contract_address : string;
  }
  [@@deriving to_yojson]
  let output_of_yojson json =
    let module T = struct
      type t = { status : string } [@@deriving of_yojson { strict = false }]

      and finished = { storage : michelson }

      and error = { error : string }
    end in
    let%ok { status } = T.of_yojson json in
    match status with
    | "success" ->
      let%ok { storage } = T.finished_of_yojson json in
      Ok storage
    | "error" ->
      let%ok T.{ error = errorMessage } = T.error_of_yojson json in
      Error errorMessage
    | _ ->
      Error
        "JSON output %s did not contain 'success' or 'error' for field `status`"
  let command = "node"
  let file = Scripts.file_fetch_storage
  let run ~rpc_node ~confirmation ~contract_address =
    let input =
      {
        rpc_node = Uri.to_string rpc_node;
        confirmation;
        contract_address = Address.to_string contract_address;
      } in
    let%await output =
      Lwt_process.pmap
        (command, [|command; file|])
        (Yojson.Safe.to_string (input_to_yojson input)) in
    match Yojson.Safe.from_string output |> output_of_yojson with
    | Ok storage -> await (Ok storage)
    | Error error -> await (Error error)
end
module Listen_transactions = struct
  type transaction = {
    entrypoint : string;
    value : michelson;
  }
  [@@deriving of_yojson]
  type output = {
    hash : string;
    transactions : transaction list;
  }
  [@@deriving of_yojson]
  module CLI = struct
    type input = {
      rpc_node : string;
      confirmation : int;
      destination : string;
    }
    [@@deriving to_yojson]
    let file = Scripts.file_listen_transactions
    let node = "node"
    let run ~context ~destination ~on_message ~on_fail =
      let send f pr data =
        let oc = pr#stdin in
        Lwt.finalize (fun () -> f oc data) (fun () -> Lwt_io.close oc) in
      let process = Lwt_process.open_process (node, [|node; file|]) in
      let input =
        {
          rpc_node = Uri.to_string context.Context.rpc_node;
          confirmation = context.required_confirmations;
          destination = Address.to_string destination;
        }
        |> input_to_yojson
        |> Yojson.Safe.to_string in
      let on_fail _exn =
        let%await _status = process#close in
        on_fail () in
      let%await () = send Lwt_io.write process input in
      let rec read_line_until_fails () =
        Lwt.catch
          (fun () ->
            let%await line = Lwt_io.read_line process#stdout in
            print_endline line;
            Yojson.Safe.from_string line
            |> output_of_yojson
            |> Result.get_ok
            |> on_message;
            read_line_until_fails ())
          on_fail in
      read_line_until_fails ()
  end
  let listen ~context ~destination ~on_message =
    let rec start () =
      Lwt.catch
        (fun () -> CLI.run ~context ~destination ~on_message ~on_fail)
        (fun _exn -> on_fail ())
    and on_fail () = start () in
    Lwt.async start
end
module Consensus = struct
  open Michelson.Michelson_v1_primitives
  open Tezos_micheline
  let commit_state_hash ~context ~block_height ~block_payload_hash ~state_hash
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
    let%await _ =
      Run_contract.run ~context ~destination:context.Context.consensus_contract
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
    match (transaction.Listen_transactions.entrypoint, transaction.value) with
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
    let%some hash = Operation_hash.of_string output.Listen_transactions.hash in
    let transactions = List.filter_map parse_transaction output.transactions in
    Some { hash; transactions }
  let listen_operations ~context ~on_operation =
    let on_message output =
      match parse_operation output with
      | Some operation -> on_operation operation
      | None -> () in
    Listen_transactions.listen ~context ~destination:context.consensus_contract
      ~on_message
  let fetch_validators ~context =
    let Context.{ rpc_node; required_confirmations; consensus_contract; _ } =
      context in
    let micheline_to_validators = function
      | Ok
          (Micheline.Prim
            (_, D_Pair, [Prim (_, D_Pair, [_; Seq (_, key_hashes)], _); _; _], _))
        ->
        List.fold_left_ok
          (fun acc k ->
            match k with
            | Micheline.String (_, k) -> (
              match Key_hash.of_string k with
              | Some k -> Ok (k :: acc)
              | None -> Error ("Failed to parse " ^ k))
            | _ -> Error "Some key_hash wasn't of type string")
          [] (List.rev key_hashes)
      | Ok _ -> Error "Failed to parse storage micheline expression"
      | Error msg -> Error msg in
    let%await micheline_storage =
      Fetch_storage.run ~confirmation:required_confirmations ~rpc_node
        ~contract_address:consensus_contract in
    Lwt.return (micheline_to_validators micheline_storage)
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
