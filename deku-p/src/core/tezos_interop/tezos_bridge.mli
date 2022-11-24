open Deku_crypto
open Deku_tezos

(* push *)
module Listen_transaction : sig
  type transaction = { entrypoint : string; value : Michelson.t }
  type t = { hash : string; transactions : transaction list }
end

(* response *)
module Inject_transaction : sig
  type error =
    | Insufficient_balance of string
    | Unknown of string
    | Consensus_contract of string
    | Several_operations of string

  type t =
    | Applied of { hash : string }
    | Failed of { hash : string option }
    | Skipped of { hash : string option }
    | Backtracked of { hash : string option }
    | Unknown of { hash : string option }
    | Error of { error : error }
end

type bridge
type t = bridge

val spawn :
  sw:Eio.Switch.t ->
  rpc_node:Uri.t ->
  secret:Secret.t ->
  destination:Address.t ->
  on_transactions:(transactions:Listen_transaction.t -> unit) ->
  bridge

val inject_transaction :
  bridge ->
  entrypoint:string ->
  payload:Data_encoding.Json.t ->
  Inject_transaction.t option
