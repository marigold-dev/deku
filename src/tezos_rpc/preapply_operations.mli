open Crypto
open Tezos

type response = Operation_content_with_result.t list

val execute :
  node_uri:Uri.t ->
  secret:Secret.t ->
  chain:Chain_id.t option ->
  protocol:Protocol_hash.t ->
  branch:Block_hash.t ->
  operations:Operation.t list ->
  (response, Error.error) result Lwt.t
