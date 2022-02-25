type response = Block_header.t Lwt_stream.t

val execute : node_uri:Uri.t -> (response, Error.error) result Lwt.t
