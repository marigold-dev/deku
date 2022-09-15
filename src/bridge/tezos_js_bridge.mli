open Deku_crypto
open Deku_tezos

(* push *)
module Listen_transaction : sig
  type transaction = { entrypoint : string; value : Michelson.t }
  [@@deriving yojson]

  type t = { hash : string; transactions : transaction list }
  [@@deriving yojson]
end

(* response *)
module Inject_transaction : sig
  type t =
    | Applied of { hash : string }
    | Failed of { hash : string option }
    | Skipped of { hash : string option }
    | Backtracked of { hash : string option }
    | Unknown of { hash : string option }
    | Error of { error : string }
end

type t

val spawn : unit -> t

val listen_transaction :
  t ->
  rpc_node:Uri.t ->
  destination:Address.t ->
  Listen_transaction.t Lwt_stream.t

val inject_transaction :
  t ->
  rpc_node:Uri.t ->
  secret:Secret.t ->
  destination:Address.t ->
  entrypoint:string ->
  payload:Yojson.Safe.t ->
  Inject_transaction.t Lwt.t
