open Deku_consensus
open Deku_indexer
open Deku_stdlib
open Deku_protocol
open Deku_concepts
open Deku_gossip
open Deku_chain
open Deku_external_vm
include Node

module Handler_utils = struct
  let input_of_body ~of_yojson (request : Piaf.Request.t) =
    try
      let body = Piaf.Body.to_string request.body |> Result.get_ok in
      body |> Yojson.Safe.from_string |> of_yojson |> Result.ok
    with exn ->
      let msg = Printexc.to_string exn in
      Error (Api_error.invalid_body msg)

  let param_of_request request param =
    Uri.get_query_param (Piaf.Request.uri request) param
end

module Api_constants = struct
  type api_constants = {
    consensus_address : Deku_tezos.Address.t;
    node_address : string;
    node_port : int;
  }

  type t = api_constants

  let make ~consensus_address ~node_port =
    { consensus_address; node_address = "127.0.0.1"; node_port }
end

type ('response, 'input) handler =
  node:node ->
  indexer:Indexer.t ->
  constants:Api_constants.t ->
  'input ->
  ('response, Api_error.t) result

let check_method method_ fn (request : Piaf.Request.t) =
  if method_ = request.meth then fn request
  else Error (Api_error.method_not_allowed request.target method_)

(* Return the nth block of the chain. *)
module Get_genesis = struct
  type response = Block.t [@@deriving yojson_of]

  let meth = `GET

  let path ~node:_ ~indexer:_ ~constants:_ =
    let handler _ = Ok (yojson_of_response Genesis.block) in
    Routes.(
      (s "api" / s "v1" / s "chain" / s "blocks" / s "genesis" /? nil)
      @--> check_method meth handler)
end

module Get_head = struct
  type response = Block.t [@@deriving yojson_of]

  let meth = `GET

  let handle ~node ~indexer:_indexer ~constants:_ () =
    let { chain; _ } = node in
    let (Chain.Chain { consensus; _ }) = chain in
    let current_block = Consensus.trusted_block consensus in
    Ok (current_block |> yojson_of_response)

  let path ~node ~indexer ~constants =
    let handler _ = handle ~node ~indexer ~constants () in
    Routes.(
      (s "api" / s "v1" / s "chain" / s "blocks" / s "head" /? nil)
      @--> check_method meth handler)
end

module Get_block_by_level_or_hash = struct
  type input = Level of Level.t | Hash of Block_hash.t
  type response = Block.t [@@deriving yojson_of]

  let meth = `GET

  let input_from_request input _request =
    let level string =
      try
        string |> Z.of_string |> N.of_z |> Option.map Level.of_n
        |> Option.map (fun level -> Level level)
      with _ -> None
    in
    let hash string =
      string |> Block_hash.of_b58 |> Option.map (fun hash -> Hash hash)
    in
    let input = [ level; hash ] |> List.find_map (fun f -> f input) in
    match input with
    | Some input -> Ok input
    | None ->
        Error
          (Api_error.invalid_parameter
             "The block parameter cannot be converted to a Level | 'head' | \
              'genesis' | Block_hash.t")

  let handle ~node ~indexer ~constants request =
    let _ = node in
    let _ = constants in

    let to_result block_opt =
      match block_opt with
      | Some block -> Ok block
      | None -> Error Api_error.block_not_found
    in

    match request with
    | Level level -> to_result (Indexer.find_block ~level indexer)
    | Hash block_hash ->
        to_result (Indexer.find_block_by_hash ~block_hash indexer)

  let path ~node ~indexer ~constants =
    let handler str request =
      Result.bind
        (input_from_request str request)
        (handle ~node ~indexer ~constants)
      |> Result.map yojson_of_response
    in
    Routes.(
      (s "api" / s "v1" / s "chain" / s "blocks" / str /? nil) @--> fun str ->
      check_method meth (handler str))
end

module Get_level = struct
  type response = { level : Level.t } [@@deriving yojson_of]

  let meth = `GET

  let handle ~node ~indexer:_ ~constants:_ () =
    let { chain; _ } = node in
    let (Chain.Chain { consensus; _ }) = chain in
    let current_block = Consensus.trusted_block consensus in
    let (Block.Block { level; _ }) = current_block in
    Ok (yojson_of_response { level })

  let path ~node ~indexer ~constants =
    let handler _request = handle ~node ~indexer ~constants () in
    Routes.(
      (s "api" / s "v1" / s "chain" / s "level" /? nil)
      @--> check_method meth handler)
end

module Get_proof = struct
  open Deku_protocol.Ledger

  type response = {
    withdrawal_handles_hash : Withdrawal_handle.hash;
    handle : Withdrawal_handle.t;
    proof : withdraw_proof;
  }
  [@@deriving yojson_of]

  let meth = `GET

  let input_from_request proof _request =
    proof |> Operation_hash.of_b58
    |> Option.to_result
         ~none:(Api_error.invalid_parameter "could not parse hash")

  let handle ~node ~indexer:_ ~constants:_ operation_hash =
    let { chain = Chain { protocol; _ }; _ } = node in
    let withdraw_proof =
      Protocol.find_withdraw_proof ~operation_hash protocol
    in
    match withdraw_proof with
    | Error _ -> Error (Api_error.invalid_parameter "Proof not found")
    | Ok (handle, proof, withdrawal_handles_hash) ->
        Ok { withdrawal_handles_hash; handle; proof }

  let path ~node ~indexer ~constants =
    let handler proof request =
      Result.bind
        (input_from_request proof request)
        (handle ~node ~indexer ~constants)
      |> Result.map yojson_of_response
    in
    Routes.(
      (s "api" / s "v1" / s "proof" / str /? nil) @--> fun str ->
      check_method meth (handler str))
end

module Get_balance = struct
  type input = {
    address : Deku_protocol.Address.t;
    ticket_id : Deku_protocol.Ticket_id.t;
  }
  [@@deriving of_yojson]

  type response = { balance : int } [@@deriving yojson_of]

  let meth = `GET

  let input_from_request address ticketer data _request =
    let%ok address =
      address |> Deku_protocol.Address.of_b58
      |> Option.to_result
           ~none:(Api_error.invalid_parameter "could not parse address")
    in
    let%ok ticketer =
      ticketer |> Deku_tezos.Contract_hash.of_string
      |> Option.to_result
           ~none:(Api_error.invalid_parameter "could not parse ticketer")
    in
    let%ok data =
      let parse_0x s =
        if String.length s < 2 then None
        else if String.starts_with ~prefix:"0x" s then
          Some (String.sub s 2 (String.length s - 2))
        else None
      in
      (* FIXME: does this handle the empty string correctly? *)
      match parse_0x data with
      | None ->
          Error
            (Api_error.invalid_parameter
               (Format.sprintf "could not parse data '%s' in 0x format" data))
      | Some s -> (
          try
            (* FIXME can this be a security issue? *)
            Ok (Hex.to_string (`Hex s))
            (* also works when s is empty *)
          with Invalid_argument _ ->
            Error
              (Api_error.invalid_parameter
                 (Format.sprintf "Invalid hex %s" data)))
    in
    let ticket_id =
      Deku_protocol.Ticket_id.make ticketer (Bytes.of_string data)
    in
    Ok { address; ticket_id }

  let handle ~node ~indexer:_ ~constants:_ { address; ticket_id } =
    let { chain = Chain { protocol = Protocol { ledger; _ }; _ }; _ } = node in
    let amount = Deku_protocol.Ledger.balance address ticket_id ledger in
    let amount = Amount.to_n amount |> N.to_z |> Z.to_int in
    Ok { balance = amount }

  let path ~node ~indexer ~constants =
    let handler address ticketer data request =
      Result.bind
        (input_from_request address ticketer data request)
        (handle ~node ~indexer ~constants)
      |> Result.map yojson_of_response
    in
    Routes.(
      (s "api" / s "v1" / s "balance" / str / str / str /? nil)
      @--> fun addr ticketer data ->
      check_method meth (handler addr ticketer data))
end

module Get_chain_info = struct
  type response = { consensus : string } [@@deriving yojson_of]

  let meth = `GET

  let handle ~node:_ ~indexer:_ ~constants () =
    let Api_constants.{ consensus_address; _ } = constants in
    Ok { consensus = Deku_tezos.Address.to_string consensus_address }
    [@@deriving yojson_of]

  let path ~node ~indexer ~constants =
    let handler _request =
      handle ~node ~indexer ~constants () |> Result.map yojson_of_response
    in
    Routes.(
      (s "api" / s "v1" / s "chain" / s "info" /? nil)
      @--> check_method meth handler)
end

module Helpers_operation_message = struct
  type response = { hash : Message_hash.t; content : Message.Content.t }
  [@@deriving yojson_of]

  let meth = `POST

  let input_from_request request =
    Handler_utils.input_of_body ~of_yojson:Operation.t_of_yojson request

  let handle ~node:_ ~indexer:_ ~constants:_ operation =
    let content = Message.Content.operation operation in
    let message, _raw_message = Message.encode ~content in
    let (Message.Message { hash; content }) = message in
    Ok { hash; content }

  let path ~node ~indexer ~constants =
    let handler request =
      Result.bind
        (input_from_request request)
        (handle ~node ~indexer ~constants)
      |> Result.map yojson_of_response
    in
    Routes.(
      (s "api" / s "v1" / s "helpers" / s "operation-messages" /? nil)
      @--> check_method meth handler)
end

module Helpers_hash_operation = struct
  (* TODO: those declarations are duplicated *)
  type operation_content =
    | Transaction of { receiver : Address.t; amount : Amount.t }
    | Noop
  [@@deriving yojson]

  type input = {
    level : Level.t;
    nonce : Nonce.t;
    source : Address.t;
    content : operation_content;
  }
  [@@deriving yojson]

  type response = { hash : Operation_hash.t } [@@deriving yojson_of]

  let meth = `POST

  let input_from_request request =
    Handler_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle ~node:_ ~indexer:_ ~constants:_ operation =
    let hash =
      operation |> yojson_of_input |> Yojson.Safe.to_string
      |> Operation_hash.hash
    in
    Ok { hash }

  let path ~node ~indexer ~constants =
    let handler request =
      Result.bind
        (input_from_request request)
        (handle ~node ~indexer ~constants)
      |> Result.map yojson_of_response
    in
    Routes.(
      (s "api" / s "v1" / s "helpers" / s "hash-operation" /? nil)
      @--> check_method meth handler)
end

(* Parse the operation and send it to the chain *)
module Post_operation = struct
  type input = Operation.t [@@deriving of_yojson]
  type response = { hash : Operation_hash.t } [@@deriving yojson_of]

  let meth = `POST

  let input_from_request request =
    Handler_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle ~env ~node:_ ~indexer:_ ~constants operation =
    let Api_constants.{ node_address = host; node_port = port; _ } =
      constants
    in
    let net = Eio.Stdenv.net env in
    let content = Message.Content.operation operation in
    let _, message_raw = Message.encode ~content in
    let (Message.Raw_message { hash = raw_expected_hash; raw_content }) =
      message_raw
    in
    let raw_expected_hash = Message_hash.to_b58 raw_expected_hash in
    let open Deku_network in
    let message = Network_message.message ~raw_expected_hash ~raw_content in
    Network_protocol.connect ~net ~host ~port @@ fun ~read:_ ~write ->
    write message;

    let (Operation.Operation { hash = operation_hash; _ }) = operation in
    { hash = operation_hash } |> yojson_of_response |> Result.ok

  let path ~env ~node ~indexer ~constants =
    let handler request =
      Result.bind
        (input_from_request request)
        (handle ~env ~node ~indexer ~constants)
    in
    Routes.(
      (s "api" / s "v1" / s "operations" /? nil) @--> check_method meth handler)
end

module Get_vm_state = struct
  type response = External_vm_protocol.State.t [@@deriving yojson_of]

  let meth = `GET

  let handle ~node ~indexer:_ ~constants:_ () =
    let { chain; _ } = node in
    let (Chain.Chain { protocol; _ }) = chain in
    let (Protocol.Protocol { vm_state; _ }) = protocol in
    vm_state |> yojson_of_response |> Result.ok

  let path ~node ~indexer ~constants =
    let handler _request = handle ~node ~indexer ~constants () in
    Routes.(
      (s "api" / s "v1" / s "state" / s "unix" /? nil)
      @--> check_method meth handler)
end
