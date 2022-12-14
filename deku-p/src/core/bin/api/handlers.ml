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
  type body
  type response

  val body_encoding : body Data_encoding.t
  val response_encoding : response Data_encoding.t
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
  type response

  val response_encoding : response Data_encoding.t
  val meth : [> `GET | `POST ]
  val route : path Routes.route
  val handler : path:path -> state:Api_state.t -> (response, Api_error.t) result
end

module Get_genesis : NO_BODY_HANDLERS = struct
  type path = unit
  type response = Repr.Block.t

  let response_encoding = Repr.Block.encoding
  let meth = `GET
  let path = Routes.(version / s "chain" / s "blocks" / s "genesis" /? nil)
  let route = Routes.(path @--> ())
  let handler ~path:_ ~state:_ = Ok (Genesis.block |> Repr.Block.of_block)
end

module Get_head : NO_BODY_HANDLERS = struct
  type path = unit
  type response = Repr.Block.t

  let response_encoding = Repr.Block.encoding
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
  type response = Block.t

  let response_encoding = Block.encoding
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
  type response = { level : Level.t }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { level } -> level)
      (fun level -> { level })
      (obj1 (req "level" Level.encoding))

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

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { withdrawal_handles_hash; handle; proof } ->
        (withdrawal_handles_hash, handle, proof))
      (fun (withdrawal_handles_hash, handle, proof) ->
        { withdrawal_handles_hash; handle; proof })
      (obj3
         (req "withdrawal_handles_hash"
            Withdrawal_handle.Withdrawal_handle_hash.encoding)
         (req "handle" Withdrawal_handle.encoding)
         (req "proof" withdraw_proof_encoding))

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

  type response = { balance : int }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { balance } -> balance)
      (fun balance -> { balance })
      (obj1 (req "balance" int8))

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
  type response = { consensus : string; in_sync : bool }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { consensus; in_sync } -> (consensus, in_sync))
      (fun (consensus, in_sync) -> { consensus; in_sync })
      (obj2 (req "consensus" string) (req "in_sync" bool))

  let meth = `GET
  let path = Routes.(version / s "chain" / s "info" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state =
    let Api_state.{ consensus_address; in_sync; _ } = state in
    Ok { consensus = Deku_tezos.Address.to_string consensus_address; in_sync }
end

module Helpers_operation_message : HANDLERS = struct
  open Deku_protocol

  type path = unit
  type body = Operation.Signed.t

  let body_encoding = Operation.Signed.encoding

  type response = { hash : Message_hash.t; content : Message.Content.t }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { hash; content } -> (hash, content))
      (fun (hash, content) -> { hash; content })
      (obj2
         (req "hash" Message_hash.encoding)
         (req "content" Message.Content.encoding))

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

  let operation_content_encoding =
    let open Data_encoding in
    union
      [
        case ~title:"Transaction" (Tag 0)
          (tup2 (dynamic_size Address.encoding) Amount.encoding)
          (function
            | Transaction { receiver; amount } -> Some (receiver, amount)
            | _ -> None)
          (fun (receiver, amount) -> Transaction { receiver; amount });
        case ~title:"Noop" (Tag 1) unit
          (function Noop -> Some () | _ -> None)
          (fun () -> Noop);
      ]

  type body = {
    level : Level.t;
    nonce : Nonce.t;
    source : Address.t;
    content : operation_content;
  }

  let body_encoding =
    let open Data_encoding in
    conv
      (fun { level; nonce; source; content } -> (level, nonce, source, content))
      (fun (level, nonce, source, content) -> { level; nonce; source; content })
      (obj4
         (req "level" (dynamic_size Level.encoding))
         (req "nonce" (dynamic_size Nonce.encoding))
         (req "source" (dynamic_size Address.encoding))
         (req "content" operation_content_encoding))

  type response = { hash : Operation_hash.t }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { hash } -> hash)
      (fun hash -> { hash })
      (obj1 (req "hash" Operation_hash.encoding))

  let meth = `POST
  let path = Routes.(version / s "helpers" / s "hash-operation" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~body:operation ~state:_ =
    let hash =
      operation
      |> assert false
      |> Data_encoding.Json.to_string |> Operation_hash.hash
    in
    Ok { hash }
end

(* Parse the operation and send it to the chain *)
module Post_operation : HANDLERS = struct
  open Deku_protocol

  type path = unit
  type body = Repr.Signed_operation.t

  let body_encoding = Repr.Signed_operation.encoding

  type response = { hash : Operation_hash.t }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { hash } -> hash)
      (fun hash -> { hash })
      (obj1 (req "hash" Operation_hash.encoding))

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
  type response = Ocaml_wasm_vm.State.t

  let response_encoding = Ocaml_wasm_vm.State.api_encoding
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
  type response = Ocaml_wasm_vm.State_entry.t option

  let response_encoding =
    let open Data_encoding in
    conv
      (fun state_entry -> state_entry)
      (fun state_entry -> state_entry)
      (option Ocaml_wasm_vm.State_entry.encoding)

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
  type response = { latency : float; tps : float }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { latency; tps } -> (latency, tps))
      (fun (latency, tps) -> { latency; tps })
      (obj2 (req "latency" float) (req "tps" float))

  let meth = `GET
  let path = Routes.(version / s "chain" / s "stats" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:_ ~state:_ =
    let Deku_metrics.{ latency; tps } = Deku_metrics.get_statistics () in
    Ok { latency; tps }
end

module Get_hexa_to_signed : HANDLERS = struct
  type path = unit
  type body = Nonce.t * Level.t * Operation.t

  let body_encoding = Operation.Initial.hash_encoding

  type response = { bytes : string }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { bytes } -> bytes)
      (fun bytes -> { bytes })
      (obj1 (req "bytes" string))

  let meth = `POST
  let path = Routes.(version / s "helpers" / s "encode-operation" /? nil)
  let route = Routes.(path @--> ())

  let hash_encoding =
    Data_encoding.tup3 Nonce.encoding Level.encoding Operation.encoding

  let handler ~path:_ ~body ~state:_ =
    let nonce, level, operation = body in
    let binary =
      Data_encoding.Binary.to_string_exn hash_encoding (nonce, level, operation)
    in
    let binary = "\x80" ^ binary in
    let bytes = binary |> Hex.of_string |> Hex.show in
    Ok { bytes }
end

module Get_receipt : NO_BODY_HANDLERS = struct
  type path = Operation_hash.t
  type response = Receipt.t

  let response_encoding = Receipt.encoding
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
  type body = { hash : Operation_hash.t }

  let body_encoding =
    let open Data_encoding in
    conv
      (fun { hash } -> hash)
      (fun hash -> { hash })
      (obj1 (req "hash" Operation_hash.encoding))

  type response = { address : Deku_ledger.Contract_address.t }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { address } -> address)
      (fun address -> { address })
      (obj1 (req "address" Deku_ledger.Contract_address.encoding))

  let meth = `POST
  let path = Routes.(version / s "helpers" / s "compute-contract-hash" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:() ~body:{ hash } ~state:_ =
    let blake2b = Operation_hash.to_blake2b hash in
    let address = Deku_ledger.Contract_address.of_user_operation_hash blake2b in
    Ok { address }
end

module Helper_compile_origination : HANDLERS = struct
  open Ocaml_wasm_vm

  type path = unit
  type body = { source : string; storage : string }

  let body_encoding =
    let open Data_encoding in
    conv
      (fun { source; storage } -> (source, storage))
      (fun (source, storage) -> { source; storage })
      (obj2 (req "source" string) (req "storage" string))

  type response = Operation_payload.t

  let response_encoding = Operation_payload.encoding
  let meth = `POST
  let path = Routes.(version / s "helpers" / s "compile-contract" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:() ~body:{ source; storage } ~state:_ =
    let tickets, init = Tunac.Compiler.compile_value storage |> Result.get_ok in
    let inputs = source in
    let wat, constants, entrypoints =
      inputs |> Tunac.Compiler.compile |> Result.get_ok
    in
    let out = Tunac.Output.make wat constants |> Result.get_ok in
    let entrypoints = entrypoints |> Option.value ~default:[] in
    Operation_payload.
      {
        tickets;
        operation =
          Operation.Originate
            {
              module_ = out.module_;
              entrypoints = Entrypoints.of_assoc entrypoints;
              constants;
              initial_storage = init;
            };
      }
    |> Result.ok
end

module Helper_compile_invocation : HANDLERS = struct
  open Ocaml_wasm_vm

  type path = unit
  type body = { address : string; expression : string }

  let body_encoding =
    let open Data_encoding in
    conv
      (fun { address; expression } -> (address, expression))
      (fun (address, expression) -> { address; expression })
      (obj2 (req "address" string) (req "expression" string))

  type response = Operation_payload.t

  let response_encoding = Operation_payload.encoding
  let meth = `POST
  let path = Routes.(version / s "helpers" / s "compile-expression" /? nil)
  let route = Routes.(path @--> ())

  let handler ~path:() ~body:{ address; expression } ~state:_ =
    let tickets, init =
      Tunac.Compiler.compile_value expression |> Result.get_ok
    in
    Operation_payload.
      {
        tickets;
        operation =
          Operation.Call
            {
              address = Deku_ledger.Address.of_b58 address |> Option.get;
              argument = init;
            };
      }
    |> Result.ok
end
