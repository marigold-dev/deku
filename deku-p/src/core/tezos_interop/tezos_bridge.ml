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

  type request = { kind : kind; rpc_node : string; destination : string }
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
    destination : string;
    entrypoint : string;
    payload : Yojson.Safe.t;
  }
  [@@deriving yojson]

  type error =
    | Insufficient_balance of string
    | Unknown of string
    | Consensus_contract of string
  [@@deriving of_yojson]

  type t =
    | Applied of { hash : string }
    (* TODO: in which cases the hash will not be present? *)
    | Failed of { hash : string option }
    | Skipped of { hash : string option }
    | Backtracked of { hash : string option }
    | Unknown of { hash : string option }
    | Error of { error : error }

  let t_of_yojson json =
    let module T = struct
      type t = { status : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type with_hash = { hash : string }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type maybe_hash = { hash : string option }
      [@@deriving of_yojson] [@@yojson.allow_extra_fields]

      type err = { error : error }
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
        let T.{ error } = T.err_of_yojson json in
        Error { error }
    | _ -> failwith "invalid status"
end

type bridge =
  | Bridge of {
      rpc_node : Uri.t;
      secret : Secret.t;
      destination : Address.t;
      (* TODO: I don't like this *)
      on_transactions : transactions:Listen_transaction.t -> unit;
      inject_transaction :
        entrypoint:string ->
        payload:Deku_stdlib.Yojson.Safe.t ->
        Inject_transaction.t option;
    }

type t = bridge

let listen_transaction ~bridge process =
  let (Bridge { rpc_node; destination; on_transactions; _ }) = bridge in
  let request =
    Listen_transaction.
      {
        kind = Listen;
        rpc_node = Uri.to_string rpc_node;
        destination = Address.to_string destination;
      }
  in
  let request = Listen_transaction.yojson_of_request request in
  let on_message message =
    let transactions = Listen_transaction.t_of_yojson message in
    on_transactions ~transactions
  in
  Js_process.listen process request ~on_message

let inject_transaction ~bridge ~entrypoint ~payload process =
  let (Bridge { rpc_node; secret; destination; _ }) = bridge in
  let input =
    Inject_transaction.
      {
        kind = Transaction;
        rpc_node = Uri.to_string rpc_node;
        secret = Secret.to_b58 secret;
        destination = Address.to_string destination;
        entrypoint;
        payload;
      }
  in
  let input = Inject_transaction.yojson_of_request input in
  let response = Js_process.request process input in
  Inject_transaction.t_of_yojson response

let spawn ~sw ~rpc_node ~secret ~destination ~on_transactions =
  let dummy_inject_transaction ~entrypoint:_ ~payload:_ = None in
  let inject_transaction_ref = ref dummy_inject_transaction in
  let bridge =
    let inject_transaction ~entrypoint ~payload =
      try !inject_transaction_ref ~entrypoint ~payload
      with exn ->
        (* TODO: this should probably be a result. Currently if
           the None case is just ignored. *)
        Logs.err (fun m -> m "inject: %s" (Printexc.to_string exn));
        None
    in
    Bridge
      { rpc_node; secret; destination; on_transactions; inject_transaction }
  in
  let rec respawn () =
    try
      let file = Scripts.file_tezos_js_bridge in
      Js_process.spawn ~file @@ fun process ->
      let inject_transaction ~entrypoint ~payload =
        Some (inject_transaction ~bridge ~entrypoint ~payload process)
      in
      inject_transaction_ref := inject_transaction;
      listen_transaction ~bridge process
    with exn ->
      Logs.err (fun m -> m "spawn: %s" (Printexc.to_string exn));
      respawn ()
  in
  let () = Eio.Fiber.fork ~sw @@ fun () -> respawn () in
  bridge

let inject_transaction bridge ~entrypoint ~payload =
  let (Bridge { inject_transaction; _ }) = bridge in
  inject_transaction ~entrypoint ~payload
