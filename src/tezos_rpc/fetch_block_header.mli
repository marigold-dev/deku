open Tezos

type response = Block_header.t

val execute :
  node_uri:Uri.t ->
  chain:Chain_id.t option ->
  block_hash:Block_hash.t option ->
  (response, Error.error) result Lwt.t
