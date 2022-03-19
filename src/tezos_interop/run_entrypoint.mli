open Crypto
open Tezos

module Process : sig
  type t
  val spawn : unit -> t
end
module Response : sig
  type transaction =
    | Applied     of { hash : string }
    | Failed      of { hash : string }
    | Skipped     of { hash : string }
    | Backtracked of { hash : string }
    | Unknown     of { hash : string }
    | Error       of string
end

val run :
  Process.t ->
  rpc_node:Uri.t ->
  secret:Secret.t ->
  required_confirmations:int ->
  destination:Address.t ->
  entrypoint:string ->
  payload:Yojson.Safe.t ->
  Response.transaction Lwt.t
