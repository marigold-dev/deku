open Helpers
open Tezos
open Http

type block_operation = {
  (* TODO: should protocl also be here? *)
  (* TODO: should signature also be here? *)
  hash : Operation_hash.t;
  chain : Chain_id.t;
  branch : Block_hash.t;
  contents : Operation_content_with_result.t list;
}
type response = block_operation list

module Decoder = struct
  type operation_content_with_result = Operation_content_with_result.t
  let operation_content_with_result_of_yojson =
    Operation_content_with_result.of_tezos_json

  type response_block_operation = block_operation = {
    hash : Operation_hash.t;
    chain : Chain_id.t; [@key "chain_id"]
    branch : Block_hash.t;
    (* TODO: can this be empty? *)
    contents : operation_content_with_result list;
  }
  [@@deriving of_yojson { strict = false }]

  let of_yojson json =
    let%ok operations = [%of_yojson: response_block_operation list list] json in
    (* TODO: tail call recursive *)
    Ok (List.concat operations)
end

let path ~chain ~block_hash =
  (* TODO: I don't like this Format.sprintf *)
  Format.sprintf "/chains/%s/blocks/%s/operations" chain block_hash

let execute ~node_uri ~chain ~block_hash =
  let chain =
    match chain with
    | Some chain -> Chain_id.to_string chain
    | None -> "main" in

  let block_hash =
    match block_hash with
    | Some block_hash -> Block_hash.to_string block_hash
    (* TODO: we could also query by height *)
    | None -> "head" in

  let path = path ~chain ~block_hash in
  http_get ~node_uri ~path ~of_yojson:Decoder.of_yojson
