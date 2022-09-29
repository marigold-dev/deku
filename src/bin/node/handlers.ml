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
  let input_of_body ~of_yojson request =
    let%await body = Dream.body request in
    let input =
      try body |> Yojson.Safe.from_string |> of_yojson |> Result.ok
      with exn ->
        let msg = Printexc.to_string exn in
        Error (Api_error.invalid_body msg)
    in
    Lwt.return input

  let param_of_request request param =
    try Dream.param request param |> Option.some with _ -> None
end

module Api_constants = struct
  type api_constants = {
    consensus_address : Deku_tezos.Address.t;
    discovery_address : Deku_tezos.Address.t;
    node_uri : Uri.t;
  }

  type t = api_constants

  let make ~consensus_address ~discovery_address ~node_uri =
    { consensus_address; discovery_address; node_uri }
end

module type HANDLER = sig
  type input
  (** The input of your handler: body, params, etc...*)

  type response [@@deriving yojson_of]
  (** The response of your handler *)

  val path : string
  (** The path of your endpoint *)

  val meth : [> `POST | `GET ]
  (** The method of your endpoint *)

  val input_from_request : Dream.request -> (input, Api_error.t) result Lwt.t
  (** Parsing function of the request to make an input *)

  val handle :
    node:node ->
    indexer:Indexer.t ->
    constants:Api_constants.t ->
    input ->
    (response, Api_error.t) result Lwt.t
  (** handler logic *)
end

(* Return the nth block of the chain. *)
module Get_genesis : HANDLER = struct
  type input = unit
  type response = Block.t [@@deriving yojson_of]

  let path = "/chain/blocks/genesis"
  let meth = `GET
  let input_from_request _ = Lwt.return_ok ()
  let handle ~node:_ ~indexer:_ ~constants:_ () = Lwt.return_ok Genesis.block
end

module Get_head : HANDLER = struct
  type input = unit
  type response = Block.t [@@deriving yojson_of]

  let path = "/chain/blocks/head"
  let meth = `GET
  let input_from_request _ = Lwt.return_ok ()

  let handle ~node ~indexer:_indexer ~constants:_ () =
    let { chain; _ } = node in
    let (Chain.Chain { consensus; _ }) = chain in
    let (Consensus.Consensus { current_block; _ }) = consensus in
    Lwt.return_ok current_block
end

module Get_block_by_level_or_hash : HANDLER = struct
  type input = Level of Level.t | Hash of Block_hash.t
  type response = Block.t [@@deriving yojson_of]

  let path = "/chain/blocks/:block"
  let meth = `GET

  let input_from_request request =
    let input = Handler_utils.param_of_request request "block" in
    let level string =
      try
        string |> Z.of_string |> N.of_z |> Option.map Level.of_n
        |> Option.map (fun level -> Level level)
      with _ -> None
    in
    let hash string =
      string |> Block_hash.of_b58 |> Option.map (fun hash -> Hash hash)
    in
    match input with
    | None -> Lwt.return_error (Api_error.missing_parameter "block")
    | Some input -> (
        let input = [ level; hash ] |> List.find_map (fun f -> f input) in
        match input with
        | Some input -> Lwt.return_ok input
        | None ->
            Lwt.return_error
              (Api_error.invalid_parameter
                 "The block parameter cannot be converted to a Level | 'head' \
                  | 'genesis' | Block_hash.t"))

  let handle ~node ~indexer ~constants request =
    let _ = node in
    let _ = constants in

    let to_result block_opt =
      match block_opt with
      | Some block -> Ok block
      | None -> Error Api_error.block_not_found
    in

    match request with
    | Level level -> Indexer.find_block ~level indexer |> Lwt.map to_result
    | Hash block_hash ->
        Indexer.find_block_by_hash ~block_hash indexer |> Lwt.map to_result
end

module Get_level : HANDLER = struct
  type input = unit
  type response = { level : Level.t } [@@deriving yojson_of]

  let path = "/chain/level"
  let meth = `GET
  let input_from_request _ = Lwt.return_ok ()

  let handle ~node ~indexer:_ ~constants:_ () =
    let { chain; _ } = node in
    let (Chain.Chain { consensus; _ }) = chain in
    let (Consensus.Consensus { current_block; _ }) = consensus in
    let (Block.Block { level; _ }) = current_block in
    Lwt.return_ok { level }
end

module Get_proof : HANDLER = struct
  open Deku_protocol.Ledger

  type input = Deku_protocol.Operation_hash.t

  type response = {
    withdrawal_handles_hash : Withdrawal_handle.hash;
    handle : Withdrawal_handle.t;
    proof : withdraw_proof;
  }
  [@@deriving yojson_of]

  let path = "/proof/:proof"
  let meth = `GET

  let input_from_request request =
    Handler_utils.param_of_request request "proof"
    |> Option.map Operation_hash.of_b58
    |> Option.join
    |> Option.to_result
         ~none:(Api_error.invalid_parameter "could not parse hash")
    |> Lwt.return

  let handle ~node ~indexer:_ ~constants:_ operation_hash =
    let { chain = Chain { protocol; _ }; _ } = node in
    let withdraw_proof =
      Protocol.find_withdraw_proof ~operation_hash protocol
    in
    match withdraw_proof with
    | Error _ ->
        Lwt.return_error (Api_error.invalid_parameter "Proof not found")
    | Ok (handle, proof, withdrawal_handles_hash) ->
        let proof = { withdrawal_handles_hash; handle; proof } in
        Lwt.return_ok proof
end

module Get_balance : HANDLER = struct
  type input = {
    address : Deku_protocol.Address.t;
    ticket_id : Deku_protocol.Ticket_id.t;
  }
  [@@deriving of_yojson]

  type response = { balance : int } [@@deriving yojson_of]

  let path = "/balance/:address/:ticketer/:data"
  let meth = `GET

  let input_from_request request =
    let open Lwt_result.Syntax in
    let* address =
      Handler_utils.param_of_request request "address"
      |> Option.map Deku_protocol.Address.of_b58
      |> Option.join
      |> Option.to_result
           ~none:(Api_error.invalid_parameter "could not parse address")
      |> Lwt.return
    in
    let* ticketer =
      Handler_utils.param_of_request request "ticketer"
      |> Option.map Deku_tezos.Contract_hash.of_string
      |> Option.join
      |> Option.to_result
           ~none:(Api_error.invalid_parameter "could not parse ticketer")
      |> Lwt.return
    in
    let* data =
      let parse_0x s =
        if String.length s < 2 then None
        else if String.starts_with ~prefix:"0x" s then
          Some (String.sub s 2 (String.length s - 2))
        else None
      in
      (* FIXME: does this handle the empty string correctly? *)
      let data = Handler_utils.param_of_request request "data" in
      match data with
      | Some s -> (
          match parse_0x s with
          | None ->
              Lwt.return
                (Error
                   (Api_error.invalid_parameter
                      (Format.sprintf "could not parse data '%s' in 0x format" s)))
          | Some s -> (
              try
                (* FIXME can this be a security issue? *)
                Lwt.return_ok (Hex.to_string (`Hex s))
                (* also works when s is empty *)
              with Invalid_argument _ ->
                Lwt.return
                  (Error
                     (Api_error.invalid_parameter
                        (Format.sprintf "Invalid hex %s" s)))))
      | None ->
          Lwt.return
            (Error
               (Api_error.invalid_parameter "could not parse data in 0x format"))
    in
    let ticket_id =
      Deku_protocol.Ticket_id.make ticketer (Bytes.of_string data)
    in
    Lwt.return_ok { address; ticket_id }

  let handle ~node ~indexer:_ ~constants:_ { address; ticket_id } =
    let { chain = Chain { protocol = Protocol { ledger; _ }; _ }; _ } = node in
    let amount = Deku_protocol.Ledger.balance address ticket_id ledger in
    let amount = Amount.to_n amount |> N.to_z |> Z.to_int in
    Lwt.return_ok { balance = amount }
end

module Get_chain_info : HANDLER = struct
  type input = unit

  type response = { consensus : string; discovery : string }
  [@@deriving yojson_of]

  let path = "/chain/info"
  let meth = `GET
  let input_from_request _ = Lwt.return_ok ()

  let handle ~node:_ ~indexer:_ ~constants () =
    let Api_constants.{ consensus_address; discovery_address; _ } = constants in
    Lwt.return_ok
      {
        consensus = Deku_tezos.Address.to_string consensus_address;
        discovery = Deku_tezos.Address.to_string discovery_address;
      }
end

module Helpers_operation_message : HANDLER = struct
  type input = Operation.t

  type response = { hash : Message_hash.t; content : Message.Content.t }
  [@@deriving yojson_of]

  let path = "/helpers/operation-messages"
  let meth = `POST

  let input_from_request request =
    Handler_utils.input_of_body ~of_yojson:Operation.t_of_yojson request

  let handle ~node:_ ~indexer:_ ~constants:_ operation =
    let content = Message.Content.operation operation in
    let message, _raw_message = Message.encode ~content in
    let (Message.Message { hash; content }) = message in
    Lwt.return_ok { hash; content }
end

module Helpers_hash_operation : HANDLER = struct
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

  let path = "/helpers/hash-operation"
  let meth = `POST

  let input_from_request request =
    Handler_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle ~node:_ ~indexer:_ ~constants:_ operation =
    let hash =
      operation |> yojson_of_input |> Yojson.Safe.to_string
      |> Operation_hash.hash
    in
    Lwt.return_ok { hash }
end

(* Parse the operation and send it to the chain *)
module Post_operation : HANDLER = struct
  open Piaf_lwt

  type input = Operation.t [@@deriving of_yojson]
  type response = { hash : Operation_hash.t } [@@deriving yojson_of]

  let meth = `POST
  let path = "/operations"

  let input_from_request request =
    Handler_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle ~node:_ ~indexer:_ ~constants operation =
    let Api_constants.{ node_uri; _ } = constants in

    let target = Uri.with_path node_uri "/messages" in

    let content = Message.Content.operation operation in
    let _message, raw_message = Message.encode ~content in

    let (Message.Raw_message { hash; raw_content }) = raw_message in
    let hash = Message_hash.to_b58 hash in

    let (Operation.Operation { hash = operation_hash; _ }) = operation in

    let headers =
      let open Piaf_lwt.Headers in
      let json = Mime_types.map_extension "json" in
      [ ("X-Raw-Expected-Hash", hash); (Well_known.content_type, json) ]
    in
    let body = Body.of_string raw_content in
    let%await post_result = Client.Oneshot.post ~headers ~body target in
    match post_result with
    | Ok _ -> Lwt.return_ok { hash = operation_hash }
    | Error err ->
        Lwt.return_error
          (Api_error.internal_error (Piaf_lwt.Error.to_string err))
end

module Get_vm_state : HANDLER = struct
  type input = unit
  type response = External_vm_protocol.State.t [@@deriving yojson_of]

  let meth = `GET
  let path = "/state/unix/"
  let input_from_request _ = Lwt.return_ok ()

  let handle ~node ~indexer:_ ~constants:_ () =
    let { chain; _ } = node in
    let (Chain.Chain { protocol; _ }) = chain in
    let (Protocol.Protocol { vm_state; _ }) = protocol in
    Lwt.return_ok vm_state
end
