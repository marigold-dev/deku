open Http

let path = "/monitor/valid_blocks"

type response = Block_header.t Lwt_stream.t
let execute ~node_uri =
  http_get_listen ~node_uri ~path ~of_yojson:Block_header.of_yojson
