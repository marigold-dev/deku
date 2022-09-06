open Deku_consensus
open Api_state
open Deku_indexer
open Deku_stdlib
open Deku_concepts

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

  val handle : input -> Api_state.t -> (response, Api_error.t) result Lwt.t
  (** handler logic *)
end

(** Listen to the deku-node for new blocks *)
module Listen_blocks : HANDLER = struct
  type input = Block.t [@@deriving of_yojson]
  type response = unit [@@deriving yojson_of]

  let path = "/listen/blocks"
  let meth = `POST

  let input_from_request request =
    Api_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle block state =
    let { indexer; _ } = state in
    let%await () = Indexer.save_block ~block indexer in
    Lwt.return_ok ()
end

(* Return the nth block of the chain. *)
module Get_genesis : HANDLER = struct
  type input = unit
  type response = Block.t [@@deriving yojson_of]

  let path = "/chain/blocks/genesis"
  let meth = `GET
  let input_from_request _ = Lwt.return_ok ()
  let handle _ _ = Lwt.return_ok Genesis.block
end

module Get_head : HANDLER = struct
  type input = unit
  type response = Block.t [@@deriving yojson_of]

  let path = "/chain/blocks/head"
  let meth = `GET
  let input_from_request _ = Lwt.return_ok ()

  let handle _ state =
    let { indexer; _ } = state in

    let to_result block_opt =
      match block_opt with
      | Some block -> Ok block
      | None -> Error Api_error.block_not_found
    in

    let%await level = Indexer.get_level indexer in
    match level with
    | None ->
        Lwt.return_error (Api_error.internal_error "level of head not found")
    | Some level -> Indexer.find_block ~level indexer |> Lwt.map to_result
end

module Get_block_by_level_or_hash : HANDLER = struct
  type input = Level of Level.t | Hash of Block_hash.t
  type response = Block.t [@@deriving yojson_of]

  let path = "/chain/blocks/:block"
  let meth = `GET

  let input_from_request request =
    let input = Api_utils.param_of_request request "block" in
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

  let handle request state =
    let { indexer; _ } = state in

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

  let handle _ state =
    let { indexer; _ } = state in
    let%await level = Indexer.get_level indexer in
    match level with
    | None -> Lwt.return_error (Api_error.internal_error "Level not found.")
    | Some level -> Lwt.return_ok { level }
end

module Get_chain_info : HANDLER = struct
  open Deku_tezos

  type input = unit

  type response = { consensus : string; discovery : string }
  [@@deriving yojson_of]

  let path = "/chain/info"
  let meth = `GET
  let input_from_request _ = Lwt.return_ok ()

  let handle () state =
    let { indexer = _; consensus; discovery } = state in
    Lwt.return_ok
      {
        consensus = Address.to_string consensus;
        discovery = Address.to_string discovery;
      }
end

module Helpers_operation_message : HANDLER = struct
  open Deku_protocol
  open Deku_gossip

  type input = Operation.t

  type response = { hash : Message_hash.t; content : Message.Content.t }
  [@@deriving yojson_of]

  let path = "/helpers/operation-messages"
  let meth = `POST

  let input_from_request request =
    Api_utils.input_of_body ~of_yojson:Operation.t_of_yojson request

  let handle operation _ =
    let content = Message.Content.operation operation in
    let message, _raw_message = Message.encode ~content in
    let (Message.Message { hash; content }) = message in
    Lwt.return_ok { hash; content }
end

module Helpers_hash_operation : HANDLER = struct
  open Deku_protocol
  open Deku_concepts

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
    Api_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle operation state =
    let _ = state in
    let hash =
      operation |> yojson_of_input |> Yojson.Safe.to_string
      |> Operation_hash.hash
    in
    Lwt.return_ok { hash }
end
