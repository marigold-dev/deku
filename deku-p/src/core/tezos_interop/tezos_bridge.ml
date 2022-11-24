open Deku_stdlib
open Deku_crypto
open Deku_tezos

module Michelson = struct
  include Michelson

  type t = Michelson.t
end

module Listen_transaction = struct
  type kind = Listen [@name "listen"]

  let kind_encoding =
    let open Data_encoding in
    conv
      (fun Listen -> [ "listen" ])
      (fun message ->
        match message with [ "listen" ] -> Listen | _ -> failwith "impossible")
      (list string)

  type request = { kind : kind; rpc_node : string; destination : string }

  let request_encoding =
    let open Data_encoding in
    conv
      (fun { kind; rpc_node; destination } -> (kind, rpc_node, destination))
      (fun (kind, rpc_node, destination) -> { kind; rpc_node; destination })
      (obj3
         (req "kind" (dynamic_size kind_encoding))
         (req "rpc_node" (dynamic_size string))
         (req "destination" string))

  type transaction = { entrypoint : string; value : Michelson.t }

  let transaction_encoding =
    let open Data_encoding in
    conv
      (fun { entrypoint; value } -> (entrypoint, value))
      (fun (entrypoint, value) -> { entrypoint; value })
      (obj2
         (req "entrypoint" (dynamic_size string))
         (req "value" (dynamic_size Michelson.expr_encoding)))

  type t = { hash : string; transactions : transaction list }

  let encoding =
    let open Data_encoding in
    conv
      (fun { hash; transactions } -> (hash, transactions))
      (fun (hash, transactions) -> { hash; transactions })
      (obj2
         (req "hash" (dynamic_size string))
         (req "transactions" (list transaction_encoding)))
end

module Inject_transaction = struct
  type kind = Transaction [@name "transaction"]

  let kind_encoding =
    let open Data_encoding in
    conv
      (fun Transaction -> [ "transaction" ])
      (fun message ->
        match message with
        | [ "transaction" ] -> Transaction
        | _ -> failwith "impossible")
      (list string)

  type request = {
    kind : kind;
    rpc_node : string;
    secret : string;
    destination : string;
    entrypoint : string;
    payload : Data_encoding.Json.t;
  }

  let request_encoding =
    let open Data_encoding in
    conv
      (fun { kind; rpc_node; secret; destination; entrypoint; payload } ->
        (kind, rpc_node, secret, destination, entrypoint, payload))
      (fun (kind, rpc_node, secret, destination, entrypoint, payload) ->
        { kind; rpc_node; secret; destination; entrypoint; payload })
      (obj6
         (req "kind" (dynamic_size kind_encoding))
         (req "rpc_node" (dynamic_size string))
         (req "secret" (dynamic_size string))
         (req "destination" (dynamic_size string))
         (req "entrypoint" (dynamic_size string))
         (req "payload" Data_encoding.Json.encoding))

  type error =
    | Insufficient_balance of string
    | Unknown of string
    | Consensus_contract of string
    | Several_operations of string

  let error_encoding =
    let open Data_encoding in
    union
      [
        case ~title:"Insufficient_balance" (Tag 0) string
          (function Insufficient_balance error -> Some error | _ -> None)
          (fun error -> Insufficient_balance error);
        case ~title:"Unknown" (Tag 1) string
          (function Unknown error -> Some error | _ -> None)
          (fun error -> Unknown error);
        case ~title:"Consensus_contract" (Tag 2) string
          (function Consensus_contract error -> Some error | _ -> None)
          (fun error -> Consensus_contract error);
        case ~title:"Several_operations" (Tag 3) string
          (function Several_operations error -> Some error | _ -> None)
          (fun error -> Several_operations error);
      ]

  type t =
    | Applied of { hash : string }
    (* TODO: in which cases the hash will not be present? *)
    | Failed of { hash : string option }
    | Skipped of { hash : string option }
    | Backtracked of { hash : string option }
    | Unknown of { hash : string option }
    | Error of { error : error }

  let encoding =
    let open Data_encoding in
    union
      [
        case ~title:"Applied" (Tag 0) string
          (function Applied { hash } -> Some hash | _ -> None)
          (fun hash -> Applied { hash });
        case ~title:"Failed" (Tag 1) (option string)
          (function Failed { hash } -> Some hash | _ -> None)
          (fun hash -> Failed { hash });
        case ~title:"Skipped" (Tag 2) (option string)
          (function Skipped { hash } -> Some hash | _ -> None)
          (fun hash -> Skipped { hash });
        case ~title:"Backtracked" (Tag 3) (option string)
          (function Backtracked { hash } -> Some hash | _ -> None)
          (fun hash -> Backtracked { hash });
        case ~title:"Unknown" (Tag 4) (option string)
          (function Unknown { hash } -> Some hash | _ -> None)
          (fun hash -> Unknown { hash });
        case ~title:"Error" (Tag 5) error_encoding
          (function Error { error } -> Some error | _ -> None)
          (fun error -> Error { error });
      ]
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
        payload:Data_encoding.Json.t ->
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
  let request =
    Data_encoding.Json.construct Listen_transaction.request_encoding request
  in
  let on_message message =
    let transactions =
      Data_encoding.Json.destruct Listen_transaction.encoding message
    in
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
  let input =
    Data_encoding.Json.construct Inject_transaction.request_encoding input
  in
  let response = Js_process.request process input in
  Data_encoding.Json.pp Format.err_formatter response;
  Data_encoding.Json.destruct Inject_transaction.encoding response

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
