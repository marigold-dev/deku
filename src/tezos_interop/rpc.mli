open Crypto
open Tezos

type 'a request_result =
  ( 'a,
    [ `Json_error         of string
    | `Piaf_body          of Piaf.Error.t
    | `Piaf_request       of Piaf.Error.t
    | `Response_of_yojson of string ] )
  result
  Lwt.t

val inject_operations :
  node_uri:Uri.t ->
  secret:Secret.t ->
  branch:Block_hash.t ->
  operations:Operation.t list ->
  Operation_hash.t request_result
