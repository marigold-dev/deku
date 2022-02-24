open Crypto
open Tezos

type response = Operation_hash.t
val execute :
  node_uri:Uri.t ->
  secret:Secret.t ->
  branch:Block_hash.t ->
  operations:Operation.t list ->
  (response, Error.error) result Lwt.t
