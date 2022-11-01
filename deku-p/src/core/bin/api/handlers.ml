open Deku_consensus
open Deku_block_storage
open Deku_stdlib
open Deku_concepts
open Deku_gossip
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
  type response = Repr.Block.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "blocks" / s "genesis" /? nil)
  let route = Routes.(path @--> ())
  let handler ~path:_ ~state:_ = Ok (Genesis.block |> Repr.Block.of_block)
end

module Get_head : NO_BODY_HANDLERS = struct
  type path = unit
  type response = Repr.Block.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "blocks" / s "head" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state =
    let Api_state.{ current_block; _ } = state in
    Ok (current_block |> Repr.Block.of_block)
end

module Get_block_by_level_or_hash : NO_BODY_HANDLERS = struct
  open Api_path

  type path = Level_or_hash.t
  type response = Yojson.Safe.t [@@deriving yojson_of]
  (*TODO: should returns a Repr.Block.t *)

  let meth = `GET

  let path =
    Routes.(version / s "chain" / s "blocks" / Level_or_hash.parser /? nil)

  let route = Routes.(path @--> fun block_or_hash -> block_or_hash)

  let handler ~path ~state =
    let Api_state.{ indexer; _ } = state in
    let open Level_or_hash in
    let block =
      match path with
      | Level level -> Block_storage.find_block_by_level ~level indexer
      | Hash block_hash -> Block_storage.find_block_by_hash ~block_hash indexer
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

module Get_proof : NO_BODY_HANDLERS = struct
  open Deku_ledger.Ledger

  type path = Operation_hash.t

  type response = {
    withdrawal_handles_hash : Withdrawal_handle.hash;
    handle : Withdrawal_handle.t;
    proof : withdraw_proof;
  }
  [@@deriving yojson_of]

  let meth = `GET

  let path =
    Routes.(version / s "proof" / Api_path.Operation_hash.parser /? nil)

  let route = Routes.(path @--> fun hash -> hash)

  let handler ~path:operation_hash ~state =
    let withdraw_proof = Api_state.find_withdraw_proof ~operation_hash state in
    match withdraw_proof with
    | Error `Unknown_operation ->
        Error (Api_error.operation_not_found operation_hash)
    | Error `Not_a_withdraw ->
        Error (Api_error.operation_is_not_a_withdraw operation_hash)
    | Ok (handle, proof, withdrawal_handles_hash) ->
        Ok { withdrawal_handles_hash; handle; proof }
end

module Get_balance : NO_BODY_HANDLERS = struct
  open Api_path

  type path = {
    address : Deku_ledger.Address.t;
    ticket_id : Deku_ledger.Ticket_id.t;
  }

  type response = { balance : int } [@@deriving yojson_of]

  let meth = `GET

  let path =
    Routes.(
      version / s "balance" / Address.parser / Ticketer.parser / Data.parser
      /? nil)

  let route =
    Routes.(
      path @--> fun address ticketer data ->
      { address; ticket_id = Deku_ledger.Ticket_id.make ticketer data })

  let handler ~path ~state =
    let { address; ticket_id } = path in
    let Api_state.{ protocol; _ } = state in
    let (Protocol { ledger; _ }) = protocol in
    let amount = Deku_ledger.Ledger.balance address ticket_id ledger in
    let amount = Amount.to_n amount |> N.to_z |> Z.to_int in
    Ok { balance = amount }
end

module Get_chain_info : NO_BODY_HANDLERS = struct
  type path = unit
  type response = { consensus : string; is_sync : bool } [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "info" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state =
    let Api_state.{ consensus_address; is_sync; _ } = state in
    Ok { consensus = Deku_tezos.Address.to_string consensus_address; is_sync }
end

module Helpers_operation_message : HANDLERS = struct
  open Deku_protocol

  type path = unit
  type body = Operation.Signed.t [@@deriving of_yojson]

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
  open Deku_ledger

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
  type body = Repr.Signed_operation.t [@@deriving of_yojson]
  type response = { hash : Operation_hash.t } [@@deriving yojson_of]

  let meth = `POST
  let path = Routes.(version / s "operations" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~body ~state =
    let Api_state.{ network; _ } = state in
    let operation = Repr.Signed_operation.to_signed body in
    match operation with
    | None -> Error Api_error.invalid_operation_signature
    | Some operation ->
        let content = Message.Content.operation operation in
        let (Message
              {
                header = _;
                content = _;
                network = Network_message { raw_header; raw_content };
              }) =
          Message.encode ~content
        in
        let (Signed_operation
              { initial = Initial_operation { hash = operation_hash; _ }; _ }) =
          operation
        in
        Network_manager.broadcast ~raw_header ~raw_content network;
        Ok { hash = operation_hash }
end

module Get_vm_state : NO_BODY_HANDLERS = struct
  type path = unit
  type response = Ocaml_wasm_vm.State.t [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "state" / s "unix" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state =
    let Api_state.{ protocol; _ } = state in
    let (Protocol.Protocol { vm_state; _ }) = protocol in
    Ok vm_state
end

module Get_vm_state_key : NO_BODY_HANDLERS = struct
  open Deku_ledger

  type path = Contract_address.t
  type response = Ocaml_wasm_vm.State_entry.t option [@@deriving yojson_of]

  let meth = `GET

  let path =
    Routes.(
      version / s "state" / s "unix" / Api_path.Contract_address.parser /? nil)

  let route = Routes.(path @--> fun key -> key)

  let handler ~path:key ~state =
    let Api_state.{ protocol; _ } = state in
    let (Protocol.Protocol { vm_state; _ }) = protocol in
    Ok
      (try Some (Ocaml_wasm_vm.State.fetch_contract vm_state key)
       with _ -> None)
end

module Get_stats : NO_BODY_HANDLERS = struct
  type path = unit
  type response = { latency : float; tps : float } [@@deriving yojson_of]

  let meth = `GET
  let path = Routes.(version / s "chain" / s "stats" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state:_ =
    let Deku_metrics.{ latency; tps } = Deku_metrics.get_statistics () in
    Ok { latency; tps }
end

module Get_hexa_to_signed : HANDLERS = struct
  type path = unit

  type body = { nonce : Nonce.t; level : Level.t; operation : Operation.t }
  [@@deriving of_yojson]

  type response = { bytes : string } [@@deriving yojson_of]

  let meth = `POST
  let path = Routes.(version / s "helpers" / s "encode-operation" /? nil)
  let route = Routes.(path @--> ())

  let hash_encoding =
    Data_encoding.tup3 Nonce.encoding Level.encoding Operation.encoding

  let handler ~path:_ ~body ~state:_ =
    let { nonce; level; operation } = body in
    let binary =
      Data_encoding.Binary.to_string_exn hash_encoding (nonce, level, operation)
    in
    let bytes = binary |> Hex.of_string |> Hex.show in
    Ok { bytes }
end

module Get_receipt : NO_BODY_HANDLERS = struct
  type path = Operation_hash.t
  type response = Receipt.t [@@deriving yojson]

  let meth = `GET

  let path =
    Routes.(version / s "operations" / Api_path.Operation_hash.parser /? nil)

  let route = Routes.(path @--> fun hash -> hash)

  let handler ~path:operation_hash ~state =
    let Api_state.{ receipts; _ } = state in
    let receipt = Operation_hash.Map.find_opt operation_hash receipts in
    match receipt with
    | Some receipt -> Ok receipt
    | None -> Error (Api_error.receipt_not_found operation_hash)
end

module Compute_contract_hash : HANDLERS = struct
  type path = unit
  type body = { hash : Operation_hash.t } [@@deriving of_yojson]

  type response = { address : Deku_ledger.Contract_address.t }
  [@@deriving yojson_of]

  let meth = `POST
  let path = Routes.(version / s "helpers" / s "compute-contract-hash" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:() ~body:{ hash } ~state:_ =
    let blake2b = Operation_hash.to_blake2b hash in
    let address = Deku_ledger.Contract_address.of_user_operation_hash blake2b in
    Ok { address }
end
