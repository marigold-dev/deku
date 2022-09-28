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

val spawn : sw:Eio.Switch.t -> t

val listen_transaction :
  t ->
  rpc_node:Uri.t ->
  required_confirmations:int ->
  destination:Address.t ->
  (Listen_transaction.t -> unit) ->
  unit

val inject_transaction :
  t ->
  rpc_node:Uri.t ->
  secret:Secret.t ->
  required_confirmations:int ->
  destination:Address.t ->
  entrypoint:string ->
  payload:Yojson.Safe.t ->
  Inject_transaction.t

val storage :
  t ->
  rpc_node:Uri.t ->
  required_confirmations:int ->
  destination:Address.t ->
  Michelson.t

val big_map_keys :
  t ->
  rpc_node:Uri.t ->
  required_confirmations:int ->
  destination:Address.t ->
  keys:Michelson.big_map_key list ->
  Yojson.Safe.t option list
