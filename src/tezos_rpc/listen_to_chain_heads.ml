open Tezos
open Http

let path ~chain =
  let chain =
    match chain with
    | Some chain -> Chain_id.to_string chain
    | None -> "main" in
  (* TODO: I don't like this Format.sprintf *)
  Format.sprintf "/monitor/heads/%s" chain

type response = Block_header.t Lwt_stream.t
let execute ~node_uri ~chain =
  let path = path ~chain in
  http_get_listen ~node_uri ~path ~of_yojson:Block_header.of_yojson
