open Helpers
open Crypto
open Tezos

module Transaction = struct
  type request = {
    rpc_node : string;
    secret : string;
    confirmation : int;
    destination : string;
    entrypoint : string;
    payload : Yojson.Safe.t;
  }
  [@@deriving to_yojson] [@@deriving of_yojson]

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

type t = Long_lived_js_process.t
let spawn () =
  let file = Scripts.file_tezos_js_bridge in
  Long_lived_js_process.spawn ~file
let transaction t ~rpc_node ~secret ~required_confirmations ~destination
    ~entrypoint ~payload =
  let request =
    Transaction.
      {
        rpc_node = Uri.to_string rpc_node;
        secret = Secret.to_string secret;
        confirmation = required_confirmations;
        destination = Address.to_string destination;
        entrypoint;
        payload;
      } in
  Long_lived_js_process.request t ~to_yojson:Transaction.request_to_yojson
    ~of_yojson:Transaction.of_yojson request
