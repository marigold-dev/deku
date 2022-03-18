open Crypto
open Tezos

type output =
  | Applied     of { hash : string }
  | Failed      of { hash : string }
  | Skipped     of { hash : string }
  | Backtracked of { hash : string }
  | Unknown     of { hash : string }
  | Error       of string

val run :
  rpc_node:Uri.t ->
  secret:Secret.t ->
  required_confirmations:int ->
  destination:Address.t ->
  entrypoint:string ->
  payload:Yojson.Safe.t ->
  output Lwt.t
