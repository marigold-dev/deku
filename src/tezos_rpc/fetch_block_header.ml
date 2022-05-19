open Tezos
open Http

type response = Block_header.t

let path ~chain ~block_hash =
  (* TODO: I don't like this Format.sprintf *)
  Format.sprintf "/chains/%s/blocks/%s/header" chain block_hash

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
  http_get ~node_uri ~path ~of_yojson:Block_header.of_yojson
