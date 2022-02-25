open Helpers
open Tezos
open Http

module Decoder = struct
  type operation_content_with_result = Operation_content_with_result.t
  let operation_content_with_result_of_yojson =
    Operation_content_with_result.of_tezos_json

  type next_operation = { contents : operation_content_with_result list }
  [@@deriving of_yojson { strict = false }]

  type t = next_operation list [@@deriving of_yojson]
end

type response = Operation_content_with_result.t list
let response_of_yojson json =
  let%ok next_operations = Decoder.of_yojson json in
  match next_operations with
  | [next_operation] -> Ok next_operation.contents
  (* TODO: leaking abstractions
      this only works because this always pass a single operation on the input *)
  | _ -> assert false

let path ~chain ~branch =
  (* TODO: I don't like this Format.sprintf *)
  Format.sprintf "/chains/%s/blocks/%s/helpers/preapply/operations" chain branch
let execute ~node_uri ~secret ~chain ~protocol ~branch ~operations =
  let path =
    let chain =
      match chain with
      | Some chain -> Chain_id.to_string chain
      | None -> "main" in
    let branch = Block_hash.to_string branch in
    path ~chain ~branch in

  let data =
    Operation.make_preapply_json ~secret ~protocol ~branch ~operations in
  http_post_data_encoding ~node_uri ~path ~of_yojson:response_of_yojson ~data
