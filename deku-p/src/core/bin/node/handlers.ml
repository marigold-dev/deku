open Deku_consensus
open Deku_indexer
open Deku_stdlib
open Deku_protocol
open Deku_concepts
open Deku_gossip
open Deku_chain
open Deku_external_vm
include Node

module Api_constants = struct
  type api_constants = {
    consensus_address : Deku_tezos.Address.t;
    node_address : string;
    node_port : int;
    identity : Identity.t;
  }

  type t = api_constants

  let make ~consensus_address ~node_port ~identity =
    { consensus_address; node_address = "127.0.0.1"; node_port; identity }
end

module Path = struct
  module Level_or_hash = struct
    type t = Level of Level.t | Hash of Block_hash.t

    let parser path =
      let serialize data =
        match data with
        | Level level -> Level.show level
        | Hash hash -> Block_hash.to_b58 hash
      in
      let parse string =
        let parse_level string =
          try
            string |> Z.of_string |> N.of_z |> Option.map Level.of_n
            |> Option.map (fun level -> Level level)
          with _ -> None
        in
        let parse_hash string =
          string |> Block_hash.of_b58 |> Option.map (fun hash -> Hash hash)
        in
        match (parse_level string, parse_hash string) with
        | None, None -> None
        | Some level, _ -> Some level
        | _, Some hash -> Some hash
      in
      Routes.custom ~serialize ~parse ~label:":level-or-hash" path
  end

  module Operation_hash = struct
    type t = Operation_hash.t

    let parser path =
      let serialize hash = Operation_hash.to_b58 hash in
      let parse string = Operation_hash.of_b58 string in
      Routes.custom ~serialize ~parse ~label:":operation-hash" path
  end

  module Address = struct
    type t = Address

    let parser path =
      let serialize address = Address.to_b58 address in
      let parse string = Address.of_b58 string in
      Routes.custom ~serialize ~parse ~label:":address" path
  end

  module Ticketer = struct
    type t = Deku_tezos.Contract_hash.t

    let parser path =
      let serialize ticketer = Deku_tezos.Contract_hash.to_b58 ticketer in
      let parse string = Deku_tezos.Contract_hash.of_b58 string in
      Routes.custom ~serialize ~parse ~label:":ticketer" path
  end

  module Data = struct
    type t = bytes

    let parser path =
      let serialize data =
        data |> Bytes.to_string |> Hex.of_string |> Hex.show
      in
      let parse string =
        Hex.to_string (`Hex string) |> Bytes.of_string |> Option.some
      in
      Routes.custom ~serialize ~parse ~label:":data" path
  end
end

let version p = Routes.(s "api" / s "v1") p

module type HANDLERS = sig
  type path
  type body [@@deriving of_yojson]
  type response [@@deriving yojson_of]

  val meth : [> `GET | `POST ]
  val route : path Routes.route

  val handler :
    env:Eio.Stdenv.t ->
    path:path ->
    body:body ->
    constants:Api_constants.t ->
    node:node ->
    indexer:Indexer.t ->
    (response, Api_error.t) result
end

module type NO_BODY_HANDLERS = sig
  type path
  type response [@@deriving yojson_of]

  val meth : [> `GET | `POST ]
  val route : path Routes.route

  val handler :
    env:Eio.Stdenv.t ->
    path:path ->
    constants:Api_constants.t ->
    node:node ->
    indexer:Indexer.t ->
    (response, Api_error.t) result
end

module Get_genesis : NO_BODY_HANDLERS = struct
  type path = unit
  type response = Block.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "blocks" / s "genesis" /? nil)
  let route = Routes.(path @--> ())
  let handler ~env:_ ~path:_ ~constants:_ ~node:_ ~indexer:_ = Ok Genesis.block
end

module Get_stats : NO_BODY_HANDLERS = struct
  type path = unit
  type response = { latency : float; tps : float } [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "stats" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env:_ ~path:_ ~constants:_ ~node:_ ~indexer:_ =
    let Deku_metrics.{ latency; tps } = Deku_metrics.get_statistics () in
    Ok { latency; tps }
end

module Get_head : NO_BODY_HANDLERS = struct
  type path = unit
  type response = Block.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "blocks" / s "head" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env:_ ~path:_ ~constants:_ ~node ~indexer:_ =
    let { chain; _ } = node in
    let (Chain.Chain { consensus; _ }) = chain in
    Consensus.trusted_block consensus |> Result.ok
end

module Get_block_by_level_or_hash : NO_BODY_HANDLERS = struct
  open Path

  type path = Level_or_hash.t
  type response = Block.t [@@deriving yojson_of]

  let meth = `GET

  let path =
    Routes.(version / s "chain" / s "blocks" / Level_or_hash.parser /? nil)

  let route = Routes.(path @--> fun block_or_hash -> block_or_hash)

  let handler ~env:_ ~path ~constants:_ ~node:_ ~indexer =
    let open Level_or_hash in
    let block =
      match path with
      | Level level -> Indexer.find_block_by_level ~level indexer
      | Hash block_hash -> Indexer.find_block_by_hash ~block_hash indexer
    in
    match block with
    | Some block -> Ok block
    | None -> Error Api_error.block_not_found
end

module Get_level : NO_BODY_HANDLERS = struct
  type path = unit
  type response = { level : Level.t } [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "level" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env:_ ~path:_ ~constants:_ ~node ~indexer:_ =
    let { chain; _ } = node in
    let (Chain.Chain { consensus; _ }) = chain in
    let current_block = Consensus.trusted_block consensus in
    let (Block.Block { level; _ }) = current_block in
    Ok { level }
end

module Get_proof : NO_BODY_HANDLERS = struct
  open Deku_protocol.Ledger

  type path = Operation_hash.t

  type response = {
    withdrawal_handles_hash : Withdrawal_handle.hash;
    handle : Withdrawal_handle.t;
    proof : withdraw_proof;
  }
  [@@deriving yojson_of]

  let meth = `GET

  let path =
    let open Path in
    Routes.(version / s "proof" / Operation_hash.parser /? nil)

  let route = Routes.(path @--> fun operation_hash -> operation_hash)

  let handler ~env:_ ~path:operation_hash ~constants:_ ~node ~indexer:_ =
    let { chain = Chain { protocol; _ }; _ } = node in
    let withdraw_proof =
      Protocol.find_withdraw_proof ~operation_hash protocol
    in
    match withdraw_proof with
    | Error _ -> Error (Api_error.invalid_parameter "Proof not found")
    | Ok (handle, proof, withdrawal_handles_hash) ->
        Ok { withdrawal_handles_hash; handle; proof }
end

module Get_balance : NO_BODY_HANDLERS = struct
  type path = {
    address : Deku_protocol.Address.t;
    ticket_id : Deku_protocol.Ticket_id.t;
  }

  type response = { balance : int } [@@deriving yojson_of]

  let meth = `GET

  let path =
    let open Path in
    Routes.(
      version / s "balance" / Address.parser / Ticketer.parser / Data.parser
      /? nil)

  let route =
    Routes.(
      path @--> fun address ticketer data ->
      { address; ticket_id = Deku_protocol.Ticket_id.make ticketer data })

  let handler ~env:_ ~path ~constants:_ ~node ~indexer:_ =
    let { address; ticket_id } = path in
    let { chain = Chain { protocol = Protocol { ledger; _ }; _ }; _ } = node in
    let amount = Deku_protocol.Ledger.balance address ticket_id ledger in
    let amount = Amount.to_n amount |> N.to_z |> Z.to_int in
    Ok { balance = amount }
end

module Get_chain_info : NO_BODY_HANDLERS = struct
  type path = unit
  type response = { consensus : string } [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "info" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env:_ ~path:_ ~constants ~node:_ ~indexer:_ =
    let Api_constants.{ consensus_address; _ } = constants in
    Ok { consensus = Deku_tezos.Address.to_string consensus_address }
end

module Helpers_operation_message : HANDLERS = struct
  type path = unit
  type body = Operation.Signed.t [@@deriving of_yojson]

  type response = { hash : Message_hash.t; content : Message.Content.t }
  [@@deriving yojson_of]

  let meth = `POST
  let path = Routes.(version / s "helpers" / s "operation-messages" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env:_ ~path:_ ~body:operation ~constants:_ ~node:_ ~indexer:_ =
    let content = Message.Content.operation operation in
    let (Message { header; content; network = _ }) = Message.encode ~content in
    let (Message_header { hash; level = _ }) = header in
    Ok { hash; content }
end

module Helpers_hash_operation : HANDLERS = struct
  type path = unit

  (* TODO: those declarations are duplicated *)
  (* TODO: check breaking change *)
  type operation_content =
    | Transaction of { receiver : Address.t; amount : Amount.t }
    | Noop
  [@@deriving yojson]

  type body = {
    level : Level.t;
    nonce : Nonce.t;
    source : Address.t;
    content : operation_content;
  }
  [@@deriving yojson]

  type response = { hash : Operation_hash.t } [@@deriving yojson_of]

  let meth = `POST
  let path = Routes.(version / s "helpers" / s "hash-operation" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env:_ ~path:_ ~body:operation ~constants:_ ~node:_ ~indexer:_ =
    let hash =
      operation |> assert false |> Yojson.Safe.to_string |> Operation_hash.hash
    in
    Ok { hash }
end

(* Parse the operation and send it to the chain *)
module Post_operation : HANDLERS = struct
  type path = unit
  type body = Operation.Signed.t [@@deriving of_yojson]
  type response = { hash : Operation_hash.t } [@@deriving yojson_of]

  let meth = `POST
  let path = Routes.(version / s "operations" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env ~path:_ ~body:operation ~constants ~node:_ ~indexer:_ =
    let Api_constants.{ node_address = host; node_port = port; identity; _ } =
      constants
    in
    let net = Eio.Stdenv.net env in
    let content = Message.Content.operation operation in
    let (Message { header = _; content = _; network }) =
      Message.encode ~content
    in
    let open Deku_network in
    let (Network_message { raw_header; raw_content }) = network in
    let message = Network_message.message ~raw_header ~raw_content in
    ( Network_protocol.Client.connect ~identity ~net ~host ~port
    @@ fun connection -> Network_protocol.Connection.write connection message );

    let (Signed_operation
          { initial = Initial_operation { hash = operation_hash; _ }; _ }) =
      operation
    in
    Ok { hash = operation_hash }
end

module Get_vm_state : NO_BODY_HANDLERS = struct
  type path = unit
  type response = External_vm_protocol.State.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "state" / s "unix" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env:_ ~path:_ ~constants:_ ~node ~indexer:_ =
    let { chain; _ } = node in
    let (Chain.Chain { protocol; _ }) = chain in
    let (Protocol.Protocol { vm_state; _ }) = protocol in
    Ok vm_state
end

module Get_vm_state_key : NO_BODY_HANDLERS = struct
  type path = string
  type response = string option [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "state" / s "unix" / str /? nil)
  let route = Routes.(path @--> fun key -> key)

  let handler ~env:_ ~path:key ~constants:_ ~node ~indexer:_ =
    let { chain; _ } = node in
    let (Chain.Chain { protocol; _ }) = chain in
    let (Protocol.Protocol { vm_state; _ }) = protocol in
    External_vm_protocol.State.get key vm_state |> Result.ok
end
