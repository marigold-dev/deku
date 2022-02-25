open Helpers
open Crypto
open Tezos

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
type status =
  | Applied
  | Other
type t =
  | Transaction     of {
      source : Key_hash.t;
      status : status;
      internal_operations : internal_operation list;
    }
  | Non_transaction

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

  let status_of_yojson json =
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

  type transaction_internal_operation_result = { status : status }
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

  type transaction_operation_result = { status : status }
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

  let of_yojson json =
    let%ok { kind } = operation_tag_of_yojson json in
    match kind with
    | Kind_transaction -> transaction_operation_of_yojson json
    | Kind_not_transaction -> Ok Non_transaction
end

let of_tezos_json = Decoder.of_yojson
