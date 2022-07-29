let post body uri =
  let headers =
    let open Piaf.Headers in
    (* TODO: maybe add json to Well_known in Piaf*)
    let json = Mime_types.map_extension "json" in
    [ (Well_known.content_type, json) ]
  in
  let body = Piaf.Body.of_string body in
  Piaf.Client.Oneshot.post ~headers ~body uri

let broadcast_json ~nodes ~endpoint ~packet =
  let endpoint = Endpoint.to_string endpoint in
  let body = Yojson.Safe.to_string packet in
  Lwt_list.iter_p
    (fun node ->
      let uri = Uri.with_path node endpoint in
      Lwt.map
        (fun _post_result ->
          (* TODO: do some/thing with this result*)
          ())
        (post body uri))
    nodes

let broadcast_json ~nodes ~endpoint ~packet =
  Lwt.async (fun () -> broadcast_json ~nodes ~endpoint ~packet)

let broadcast_packet ~nodes ~endpoint ~packet =
  let packet = Packet.yojson_of_t packet in
  broadcast_json ~nodes ~endpoint ~packet
