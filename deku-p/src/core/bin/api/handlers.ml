open Deku_consensus
open Deku_indexer
open Deku_stdlib
open Deku_concepts
open Deku_gossip
open Deku_external_vm
open Deku_network
open Deku_protocol

let version p = Routes.(s "api" / s "v1") p

module type HANDLERS = sig
  type path
  type body [@@deriving of_yojson]
  type response [@@deriving yojson_of]

  val meth : [> `GET | `POST ]
  val route : path Routes.route

  val handler :
    path:path ->
    body:body ->
    state:Api_state.t ->
    (response, Api_error.t) result
end

module type NO_BODY_HANDLERS = sig
  type path
  type response [@@deriving yojson_of]

  val meth : [> `GET | `POST ]
  val route : path Routes.route
  val handler : path:path -> state:Api_state.t -> (response, Api_error.t) result
end

module Get_genesis : NO_BODY_HANDLERS = struct
  type path = unit
  type response = Block.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "blocks" / s "genesis" /? nil)
  let route = Routes.(path @--> ())
  let handler ~path:_ ~state:_ = Ok Genesis.block
end

module Get_head : NO_BODY_HANDLERS = struct
  type path = unit
  type response = Block.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "blocks" / s "head" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state =
    let Api_state.{ current_block; _ } = state in
    Ok current_block
end

module Get_block_by_level_or_hash : NO_BODY_HANDLERS = struct
  open Api_path

  type path = Level_or_hash.t
  type response = Yojson.Safe.t [@@deriving yojson_of]

  let meth = `GET

  let path =
    Routes.(version / s "chain" / s "blocks" / Level_or_hash.parser /? nil)

  let route = Routes.(path @--> fun block_or_hash -> block_or_hash)

  let handler ~path ~state =
    let Api_state.{ indexer; _ } = state in
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

  let handler ~path:_ ~state =
    let Api_state.{ current_block; _ } = state in
    let (Block.Block { level; _ }) = current_block in
    Ok { level }
end

(* module Get_proof : NO_BODY_HANDLERS = struct
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

     let handler ~path:operation_hash ~state:_ =
       let { chain = Chain { protocol; _ }; _ } = node in
       let withdraw_proof =
         Protocol.find_withdraw_proof ~operation_hash protocol
       in
       match withdraw_proof with
       | Error _ -> Error (Api_error.invalid_parameter "Proof not found")
       | Ok (handle, proof, withdrawal_handles_hash) ->
           Ok { withdrawal_handles_hash; handle; proof }
   end *)

(* module Get_balance : NO_BODY_HANDLERS = struct
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

     let handler ~path ~state:_ =
       let { address; ticket_id } = path in
       let { chain = Chain { protocol = Protocol { ledger; _ }; _ }; _ } = node in
       let amount = Deku_protocol.Ledger.balance address ticket_id ledger in
       let amount = Amount.to_n amount |> N.to_z |> Z.to_int in
       Ok { balance = amount }
   end *)

module Get_chain_info : NO_BODY_HANDLERS = struct
  type path = unit
  type response = { consensus : string } [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "info" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state =
    let Api_state.{ consensus_address; _ } = state in
    Ok { consensus = Deku_tezos.Address.to_string consensus_address }
end

module Helpers_operation_message : HANDLERS = struct
  open Deku_protocol

  type path = unit
  type body = Operation.t [@@deriving of_yojson]

  type response = { hash : Message_hash.t; content : Message.Content.t }
  [@@deriving yojson_of]

  let meth = `POST
  let path = Routes.(version / s "helpers" / s "operation-messages" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~body:operation ~state:_ =
    let content = Message.Content.operation operation in
    let (Message { header; content; network = _ }) = Message.encode ~content in
    let (Message_header { hash; level = _ }) = header in
    Ok { hash; content }
end

module Helpers_hash_operation : HANDLERS = struct
  open Deku_protocol

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

  let handler ~path:_ ~body:operation ~state:_ =
    let hash =
      operation |> assert false |> Yojson.Safe.to_string |> Operation_hash.hash
    in
    Ok { hash }
end

(* Parse the operation and send it to the chain *)
module Post_operation : HANDLERS = struct
  open Deku_protocol

  type path = unit
  type body = Operation.t [@@deriving of_yojson]
  type response = { hash : Operation_hash.t } [@@deriving yojson_of]

  let meth = `POST
  let path = Routes.(version / s "operations" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~body:operation ~state =
    let Api_state.{ network; _ } = state in
    let content = Message.Content.operation operation in
    let (Message
          {
            header = _;
            content = _;
            network = Network_message { raw_header; raw_content };
          }) =
      Message.encode ~content
    in
    let (Operation.Operation { hash = operation_hash; _ }) = operation in
    Network_manager.broadcast ~raw_header ~raw_content network;
    Ok { hash = operation_hash }
end

module Get_vm_state : NO_BODY_HANDLERS = struct
  type path = unit
  type response = External_vm_protocol.State.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "state" / s "unix" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state =
    let Api_state.{ protocol; _ } = state in
    let (Protocol.Protocol { vm_state; _ }) = protocol in
    Ok vm_state
end
