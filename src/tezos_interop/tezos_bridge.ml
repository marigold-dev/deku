open Helpers
open Crypto
open Tezos

module Michelson = struct
  include Michelson
  type t = Michelson.t
  let of_yojson json =
    let%ok json = Yojson.Safe.to_string json |> Data_encoding.Json.from_string in
    try Ok (Data_encoding.Json.destruct Michelson.expr_encoding json) with
    | _ -> Error "invalid json"
  let big_map_key_to_yojson (Key_hash key_hash) : Yojson.Safe.t =
    `Assoc
      [
        ("key", `Assoc [("string", `String (Key_hash.to_string key_hash))]);
        ("type", `Assoc [("prim", `String "key_hash")]);
      ]
end
module Listen_transaction = struct
  type kind = Listen
  let kind_to_yojson Listen = `String "listen"
  type request = {
    kind : kind;
    rpc_node : string;
    confirmation : int;
    destination : string;
  }
  [@@deriving to_yojson]

  type transaction = {
    entrypoint : string;
    value : Michelson.t;
  }
  [@@deriving of_yojson]
  type t = {
    hash : string;
    transactions : transaction list;
  }
  [@@deriving of_yojson]
end
module Inject_transaction = struct
  type kind = Transaction
  let kind_to_yojson Transaction = `String "transaction"
  type request = {
    kind : kind;
    rpc_node : string;
    secret : string;
    confirmation : int;
    destination : string;
    entrypoint : string;
    payload : Yojson.Safe.t;
  }
  [@@deriving to_yojson]

  type t =
    | Applied     of { hash : string }
    (* TODO: in which cases the hash will not be present? *)
    | Failed      of { hash : string option }
    | Skipped     of { hash : string option }
    | Backtracked of { hash : string option }
    | Unknown     of { hash : string option }
    | Error       of { error : string }

  let of_yojson json =
    let module T = struct
      type t = { status : string } [@@deriving of_yojson { strict = false }]
      type with_hash = { hash : string }
      [@@deriving of_yojson { strict = false }]
      type maybe_hash = { hash : string option }
      [@@deriving of_yojson { strict = false }]
      type error = { error : string } [@@deriving of_yojson { strict = false }]
    end in
    let other make =
      let%ok { hash } = T.maybe_hash_of_yojson json in
      Ok (make hash) in

    let%ok { status } = T.of_yojson json in
    match status with
    | "applied" ->
      let%ok { hash } = T.with_hash_of_yojson json in
      Ok (Applied { hash })
    | "failed" -> other (fun hash -> Failed { hash })
    | "skipped" -> other (fun hash -> Skipped { hash })
    | "backtracked" -> other (fun hash -> Backtracked { hash })
    | "unknown" -> other (fun hash -> Unknown { hash })
    | "error" ->
      let%ok { error } = T.error_of_yojson json in
      Ok (Error { error })
    | _ -> Error "invalid status"
end

module Storage = struct
  type kind = Storage
  let kind_to_yojson Storage = `String "storage"
  type request = {
    kind : kind;
    rpc_node : string;
    confirmation : int;
    destination : string;
  }
  [@@deriving to_yojson]

  let of_yojson json =
    let module T = struct
      type t = { status : string } [@@deriving of_yojson { strict = false }]
      type success = { storage : Michelson.t }
      [@@deriving of_yojson { strict = false }]
      type error = { error : string } [@@deriving of_yojson { strict = false }]
    end in
    let%ok { status } = T.of_yojson json in
    match status with
    | "success" ->
      let%ok { storage } = T.success_of_yojson json in
      Ok (Ok storage)
    | "error" ->
      let%ok { error } = T.error_of_yojson json in
      Ok (Error error)
    | _ -> Error "invalid status"
end

module Big_map = struct
  type kind = Big_map
  let kind_to_yojson Big_map = `String "big_map"
  type request = {
    kind : kind;
    rpc_node : string;
    confirmation : int;
    destination : string;
    key : Michelson.big_map_key;
  }
  [@@deriving to_yojson]

  let of_yojson json =
    let module T = struct
      type t = { status : string } [@@deriving of_yojson { strict = false }]
      type success = { value : Michelson.t }
      [@@deriving of_yojson { strict = false }]
      type error = { error : string } [@@deriving of_yojson { strict = false }]
    end in
    let%ok { status } = T.of_yojson json in
    match status with
    | "success" ->
      let%ok { value } = T.success_of_yojson json in
      Ok (Ok value)
    | "error" ->
      let%ok { error } = T.error_of_yojson json in
      Ok (Error error)
    | _ -> Error "invalid status"
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
        destination = Address.to_string destination;
      } in
  Long_lived_js_process.listen t ~to_yojson:Listen_transaction.request_to_yojson
    ~of_yojson:Listen_transaction.of_yojson request

let inject_transaction t ~rpc_node ~secret ~required_confirmations ~destination
    ~entrypoint ~payload =
  let request =
    Inject_transaction.
      {
        kind = Transaction;
        rpc_node = Uri.to_string rpc_node;
        secret = Secret.to_string secret;
        confirmation = required_confirmations;
        destination = Address.to_string destination;
        entrypoint;
        payload;
      } in
  Long_lived_js_process.request t
    ~to_yojson:Inject_transaction.request_to_yojson
    ~of_yojson:Inject_transaction.of_yojson request

let storage t ~rpc_node ~required_confirmations ~destination =
  let request =
    Storage.
      {
        kind = Storage;
        rpc_node = Uri.to_string rpc_node;
        confirmation = required_confirmations;
        destination = Address.to_string destination;
      } in
  Long_lived_js_process.request t ~to_yojson:Storage.request_to_yojson
    ~of_yojson:Storage.of_yojson request

let big_map t ~rpc_node ~required_confirmations ~destination ~key =
  let request =
    Big_map.
      {
        kind = Big_map;
        rpc_node = Uri.to_string rpc_node;
        confirmation = required_confirmations;
        destination = Address.to_string destination;
        key;
      } in
  Long_lived_js_process.request t ~to_yojson:Big_map.request_to_yojson
    ~of_yojson:Big_map.of_yojson request
