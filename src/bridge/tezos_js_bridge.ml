open Deku_stdlib
open Deku_crypto
open Deku_tezos

module Michelson = struct
  include Michelson

  type t = Michelson.t

  let t_of_yojson json =
    match Yojson.Safe.to_string json |> Data_encoding.Json.from_string with
    | Ok json -> Data_encoding.Json.destruct Michelson.expr_encoding json
    | Error err -> failwith err

  let yojson_of_t t =
    Data_encoding.Json.construct Michelson.expr_encoding t
    |> Data_encoding.Json.to_string |> Yojson.Safe.from_string
end

module Listen_transaction = struct
  type kind = Listen [@name "listen"] [@@deriving yojson]

  type request = {
    kind : kind;
    rpc_node : string;
    confirmation : int;
    destination : string;
  }
  [@@deriving yojson]

  type transaction = { entrypoint : string; value : Michelson.t }
  [@@deriving yojson]

  type t = { hash : string; transactions : transaction list }
  [@@deriving yojson]
end

module Inject_transaction = struct
  type kind = Transaction [@name "transaction"] [@@deriving yojson]

  type request = {
    kind : kind;
    rpc_node : string;
    secret : string;
    confirmation : int;
    destination : string;
    entrypoint : string;
    payload : Yojson.Safe.t;
  }
  [@@deriving yojson]

  type t =
    | Applied of { hash : string }
    (* TODO: in which cases the hash will not be present? *)
    | Failed of { hash : string option }
    | Skipped of { hash : string option }
    | Backtracked of { hash : string option }
    | Unknown of { hash : string option }
    | Error of { error : string }

  let of_yojson json =
    let module T = struct
      type t = { status : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type with_hash = { hash : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type maybe_hash = { hash : string option }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type error = { error : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]
    end in
    let other make =
      let T.{ hash } = T.maybe_hash_of_yojson json in
      make hash
    in

    let T.{ status } = T.t_of_yojson json in
    match status with
    | "applied" ->
        let (T.{ hash } : T.with_hash) = T.with_hash_of_yojson json in
        Applied { hash }
    | "failed" -> other (fun hash -> Failed { hash })
    | "skipped" -> other (fun hash -> Skipped { hash })
    | "backtracked" -> other (fun hash -> Backtracked { hash })
    | "unknown" -> other (fun hash -> Unknown { hash })
    | "error" ->
        let T.{ error } = T.error_of_yojson json in
        Error { error }
    | _ -> failwith "invalid status"
end

type t = Long_lived_js_process.t

let spawn () =
  let file = Scripts.file_tezos_js_bridge in
  Long_lived_js_process.spawn ~file

let listen_transaction t ~rpc_node ~required_confirmations ~destination =
  let request =
    Listen_transaction.
      {
        kind = Listen;
        rpc_node = Uri.to_string rpc_node;
        confirmation = required_confirmations;
        destination = Address.to_b58 destination;
      }
  in
  Long_lived_js_process.listen t ~to_yojson:Listen_transaction.yojson_of_request
    ~of_yojson:Listen_transaction.t_of_yojson request

let inject_transaction t ~rpc_node ~secret ~required_confirmations ~destination
    ~entrypoint ~payload =
  let request =
    Inject_transaction.
      {
        kind = Transaction;
        rpc_node = Uri.to_string rpc_node;
        secret = Secret.to_b58 secret;
        confirmation = required_confirmations;
        destination = Address.to_b58 destination;
        entrypoint;
        payload;
      }
  in
  Long_lived_js_process.request t
    ~to_yojson:Inject_transaction.yojson_of_request
    ~of_yojson:Inject_transaction.of_yojson request
