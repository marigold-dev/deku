open Helpers
open Crypto
open Tezos

type error =
  | Json_error         of string
  | Piaf_body          of Piaf.Error.t
  | Piaf_request       of Piaf.Error.t
  | Response_of_yojson of string

type 'a method_ =
  | GET : unit method_
  | POST : string method_
let http_request (type a) ~uri ~(method_ : a method_) (data : a) =
  let open Piaf in
  let%await response =
    match method_ with
    | GET -> Client.Oneshot.get uri
    | POST ->
      let body = Body.of_string data in
      Client.Oneshot.post ~body uri in
  match response with
  | Ok response -> (
    let%await body = Piaf.Body.to_string response.body in
    match body with
    | Ok body -> await (Ok body)
    | Error err -> await (Error (Piaf_body err)))
  | Error err -> await (Error (Piaf_request err))

let http_request ~node_uri ~path ~method_ response_of_yojson data =
  let uri = Uri.with_path node_uri path in
  let%await body = http_request ~uri ~method_ data in
  match body with
  | Ok body -> (
    try
      let json = Yojson.Safe.from_string body in
      match response_of_yojson json with
      | Ok response -> await (Ok response)
      | Error err -> await (Error (Response_of_yojson err))
    with
    | Yojson.Json_error err -> await (Error (Json_error err)))
  | Error err -> await (Error err)

let http_get ~node_uri ~path response_of_yojson =
  http_request ~node_uri ~path ~method_:GET response_of_yojson ()
let http_post ~node_uri ~path response_of_yojson data =
  http_request ~node_uri ~path ~method_:POST response_of_yojson data

let inject_operations_path = "/injection/operation"
type inject_operations_response = Operation_hash.t [@@deriving yojson]

let inject_operations ~node_uri ~secret ~branch ~operations =
  let signed_forged_operation = Operation.forge ~secret ~branch ~operations in
  let path = inject_operations_path in
  (* TODO: should I use ?async *)
  http_post ~node_uri ~path inject_operations_response_of_yojson
    signed_forged_operation

module Chain_id = struct
  type t
  let of_yojson _ = assert false
  let to_string _ = assert false
end
let block_operations_path ~chain_id ~block_hash =
  Format.sprintf "/chains/%s/blocks/%s/operations"
    (Chain_id.to_string chain_id)
    (Block_hash.to_string block_hash)

type parameters = {
  entrypoint : string;
  value : Michelson.t;
}
type internal_operation =
  | Internal_transaction     of {
      sender : Address.t;
      destination : Address.t;
      parameters : parameters option;
    }
  | Internal_non_transaction

type operation_status =
  | Applied
  | Other
type operation =
  | Transaction     of {
      source : Key_hash.t;
      status : operation_status;
      internal_operations : internal_operation list;
    }
  | Non_transaction
type block_operation = {
  (* TODO: should protocl also be here? *)
  (* TODO: should signature also be here? *)
  hash : Operation_hash.t;
  chain : Chain_id.t;
  branch : Block_hash.t;
  contents : operation list;
}

let block_transactions ~node_uri ~chain_id ~block_hash =
  (* TODO: what to do when some operations are not known?
          Let's say by a bug or because the format changed? *)
  (* TODO: also, how is the failure mode, exceptions or result???*)
  let module Response = struct
    type kind =
      | Kind_transaction
      | Kind_not_transaction
    let kind_of_yojson json =
      (* TODO: probably can be done better *)
      let%ok kind = [%of_yojson: string] json in
      match kind with
      | "transaction" -> Ok Kind_transaction
      | _ -> Ok Kind_not_transaction

    let operation_status_of_yojson json =
      let%ok status = [%of_yojson: string] json in
      match status with
      | "applied" -> Ok Applied
      | _ -> Ok Other

    type parameters_base = {
      entrypoint : string;
      value : Yojson.Safe.t;
    }
    [@@deriving of_yojson]

    let parameters_of_yojson json =
      let%ok { entrypoint; value } = parameters_base_of_yojson json in
      (* TODO: this is completely cursed and we shuold switch to Data_encoding *)
      let%ok value =
        Yojson.Safe.to_string value |> Data_encoding.Json.from_string in
      (* TODO: this fails with exception *)
      let value = Data_encoding.Json.destruct Michelson.expr_encoding value in
      Ok ({ entrypoint; value } : parameters)

    type transaction_internal_operation_result = { status : operation_status }
    [@@deriving of_yojson { strict = false }]

    type transaction_internal_operation = {
      source : Address.t;
      destination : Address.t;
      parameters : parameters option;
      result : transaction_internal_operation_result;
    }
    [@@deriving of_yojson { strict = false }]

    type internal_operation_tag = { kind : kind }
    [@@deriving of_yojson { strict = false }]

    let transaction_internal_operation_of_yojson json =
      let%ok { source; destination; parameters; result } =
        transaction_internal_operation_of_yojson json in
      (*TODO: what to do with this status? *)
      let _ = result.status in
      Ok (Internal_transaction { sender = source; destination; parameters })

    let internal_operation_of_yojson json =
      let%ok { kind } = internal_operation_tag_of_yojson json in
      match kind with
      | Kind_transaction -> transaction_internal_operation_of_yojson json
      | Kind_not_transaction -> Ok Internal_non_transaction

    type transaction_operation_result = { status : operation_status }
    [@@deriving of_yojson { strict = false }]

    type transaction_operation_metadata = {
      operation_result : transaction_operation_result;
      (* TODO: default when list is not present *)
      internal_operation_results : internal_operation list;
    }
    [@@deriving of_yojson { strict = false }]

    type transaction_operation = {
      source : Key_hash.t;
      destination : Address.t;
      parameters : parameters option;
      metadata : transaction_operation_metadata;
    }
    [@@deriving of_yojson { strict = false }]

    type operation_tag = { kind : kind }
    [@@deriving of_yojson { strict = false }]

    let transaction_operation_of_yojson json =
      let%ok { source; destination; parameters; metadata } =
        transaction_operation_of_yojson json in
      (* TODO: does applied here represents also the status of all the internal_transactions? *)
      let initial_transaction =
        Internal_transaction
          { sender = Implicit source; destination; parameters } in
      let internal_operations =
        initial_transaction :: metadata.internal_operation_results in

      Ok
        (Transaction
           {
             source;
             status = metadata.operation_result.status;
             internal_operations;
           })

    let operation_of_yojson json =
      let%ok { kind } = operation_tag_of_yojson json in
      match kind with
      | Kind_transaction -> transaction_operation_of_yojson json
      | Kind_not_transaction -> Ok Non_transaction

    type response_block_operation = block_operation = {
      hash : Operation_hash.t;
      chain : Chain_id.t;
      branch : Block_hash.t;
      (* TODO: can this be empty? *)
      contents : operation list;
    }
    [@@deriving of_yojson { strict = false }]

    type t = response_block_operation list
    let of_yojson json =
      let%ok operations =
        [%of_yojson: response_block_operation list list] json in
      (* TODO: tail call recursive *)
      Ok (List.concat operations)
  end in
  let path = block_operations_path ~chain_id ~block_hash in
  http_get ~node_uri ~path Response.of_yojson