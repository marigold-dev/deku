open Deku_stdlib

let post body uri =
  let headers =
    let open Piaf.Headers in
    (* TODO: maybe add json to Well_known in Piaf*)
    let json = Mime_types.map_extension "json" in
    [ (Well_known.content_type, json) ]
  in
  let body = Piaf.Body.of_string body in
  Piaf.Client.Oneshot.post ~headers ~body uri

let broadcast_json (type a) ~nodes ~(endpoint : a Endpoint.t) ~packet =
  (* TODO: this definitely should not be here *)
  let%await () =
    match endpoint with Blocks -> Lwt_unix.sleep 1.0 | _ -> Lwt.return_unit
  in
  let endpoint = Endpoint.to_string endpoint in
  let body = Yojson.Safe.to_string packet in

  Lwt_list.iter_p
    (fun node ->
      let uri = Uri.with_path node endpoint in
      let%await post_result = post body uri in
      (* TODO: do some/thing with this result*)
      match post_result with
      | Ok response -> (
          let body = response.body in
          let%await drain_result = Piaf.Body.drain body in
          match drain_result with
          | Ok () -> Lwt.return_unit
          | Error _err ->
              (* TODO: do something with this error *) Lwt.return_unit)
      | Error _err -> (* TODO: do something with this error *) Lwt.return_unit)
    nodes

let broadcast_json ~nodes ~endpoint ~packet =
  Lwt.async (fun () -> broadcast_json ~nodes ~endpoint ~packet)

let broadcast_packet ~nodes ~endpoint ~packet =
  let packet = Packet.yojson_of_t packet in
  broadcast_json ~nodes ~endpoint ~packet
