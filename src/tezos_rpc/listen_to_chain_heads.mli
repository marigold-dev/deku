open Tezos

type response = Block_header.t Lwt_stream.t

val execute :
  node_uri:Uri.t ->
  chain:Chain_id.t option ->
  (response, Error.error) result Lwt.t
