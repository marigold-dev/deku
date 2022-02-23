open Tezos

type block_operation = {
  (* TODO: should protocl also be here? *)
  (* TODO: should signature also be here? *)
  hash : Operation_hash.t;
  chain : Chain_id.t;
  branch : Block_hash.t;
  contents : Operation_content_with_result.t list;
}
type response = block_operation list

val execute :
  node_uri:Uri.t ->
  chain:Chain_id.t option ->
  block_hash:Block_hash.t option ->
  (response, Error.error) result Lwt.t
