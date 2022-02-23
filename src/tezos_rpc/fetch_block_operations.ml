open Helpers
open Crypto
open Tezos
open Http

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
  (* if one of the internal operations the status will be "backtracked" *)
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
type response = block_operation list

module Decoder = struct
  (* TODO: what to do when some operations are not known?
        Let's say by a bug or because the format changed? *)
  (* TODO: also, how is the failure mode, exceptions or result???*)
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
    parameters : parameters option; [@default None]
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
    internal_operation_results : internal_operation list; [@default []]
  }
  [@@deriving of_yojson { strict = false }]

  type transaction_operation = {
    source : Key_hash.t;
    destination : Address.t;
    parameters : parameters option; [@default None]
    metadata : transaction_operation_metadata;
  }
  [@@deriving of_yojson { strict = false }]

  type operation_tag = { kind : kind } [@@deriving of_yojson { strict = false }]

  let transaction_operation_of_yojson json =
    let%ok { source; destination; parameters; metadata } =
      transaction_operation_of_yojson json in
    (* TODO: does applied here represents also the status of all the internal_transactions? *)
    let initial_transaction =
      Internal_transaction { sender = Implicit source; destination; parameters }
    in
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
    chain : Chain_id.t; [@key "chain_id"]
    branch : Block_hash.t;
    (* TODO: can this be empty? *)
    contents : operation list;
  }
  [@@deriving of_yojson { strict = false }]

  let of_yojson json =
    let%ok operations = [%of_yojson: response_block_operation list list] json in
    (* TODO: tail call recursive *)
    Ok (List.concat operations)
end

let path ~chain ~block_hash =
  (* TODO: I don't like this Format.sprintf *)
  Format.sprintf "/chains/%s/blocks/%s/operations" chain block_hash

let execute ~node_uri ~chain ~block_hash =
  let chain =
    match chain with
    | Some chain -> Chain_id.to_string chain
    | None -> "main" in

  let block_hash =
    match block_hash with
    | Some block_hash -> Block_hash.to_string block_hash
    (* TODO: we could also query by height *)
    | None -> "head" in

  let path = path ~chain ~block_hash in
  http_get ~node_uri ~path ~of_yojson:Decoder.of_yojson
