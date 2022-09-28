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

  let yojson_of_big_map_key (Key_hash key_hash) : Yojson.Safe.t =
    `String (Key_hash.to_b58 key_hash)

  let big_map_key_of_yojson json =
    match json with
    | `String string ->
        let key_hash = Key_hash.of_b58 string |> Option.get in
        Key_hash key_hash
    | _ -> failwith "big_map_key was not properly serialized"
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

module Storage = struct
  type kind = Storage [@@deriving yojson]

  type request = {
    kind : kind;
    rpc_node : string;
    confirmation : int;
    destination : string;
  }
  [@@deriving yojson]

  let of_yojson json =
    let module T = struct
      type t = { status : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type success = { storage : Michelson.t }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type error = { error : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]
    end in
    let T.{ status } = T.t_of_yojson json in
    match status with
    | "success" ->
        let T.{ storage } = T.success_of_yojson json in
        storage
    | "error" ->
        let T.{ error } = T.error_of_yojson json in
        failwith error
    | _ -> failwith "invalid status"
end

module Big_map_keys = struct
  type kind = Big_map_keys [@name "big_map_keys"] [@@deriving yojson]

  type request = {
    kind : kind;
    rpc_node : string;
    confirmation : int;
    destination : string;
    keys : Michelson.big_map_key list;
  }
  [@@deriving yojson]

  let of_yojson json =
    let module T = struct
      type t = { status : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      (*
         TODO: Yojson.Safe.t is taking the place of a "well-formatted json object"
         See: https://github.com/ecadlabs/taquito/blob/fcee40a6235ed0dd56ff6a8f3e64fb9d0c16cab3/packages/taquito/src/contract/big-map.ts#L46
         It would be better to parse it to a micheline node.
      *)
      type success = { values : Yojson.Safe.t option list }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type error = { error : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]
    end in
    let T.{ status } = T.t_of_yojson json in
    match status with
    | "success" ->
        let T.{ values } = T.success_of_yojson json in
        values
    | "error" ->
        let T.{ error } = T.error_of_yojson json in
        failwith error
    | _ -> failwith "invalid status"
end

type t = Long_lived_js_process.t

let spawn ~sw =
  let file = Scripts.file_tezos_js_bridge in
  Long_lived_js_process.spawn ~sw ~file

let listen_transaction t ~rpc_node ~required_confirmations ~destination =
  let request =
    Listen_transaction.
      {
        kind = Listen;
        rpc_node = Uri.to_string rpc_node;
        confirmation = required_confirmations;
        destination = Address.to_string destination;
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
        destination = Address.to_string destination;
        entrypoint;
        payload;
      }
  in
  Long_lived_js_process.request t
    ~to_yojson:Inject_transaction.yojson_of_request
    ~of_yojson:Inject_transaction.of_yojson request

let storage t ~rpc_node ~required_confirmations ~destination =
  let request =
    Storage.
      {
        kind = Storage;
        rpc_node = Uri.to_string rpc_node;
        confirmation = required_confirmations;
        destination = Address.to_string destination;
      }
  in
  Long_lived_js_process.request t ~to_yojson:Storage.yojson_of_request
    ~of_yojson:Storage.of_yojson request

let big_map_keys t ~rpc_node ~required_confirmations ~destination ~keys =
  let request =
    Big_map_keys.
      {
        kind = Big_map_keys;
        rpc_node = Uri.to_string rpc_node;
        confirmation = required_confirmations;
        destination = Address.to_string destination;
        keys;
      }
  in
  Long_lived_js_process.request t ~to_yojson:Big_map_keys.yojson_of_request
    ~of_yojson:Big_map_keys.of_yojson request
