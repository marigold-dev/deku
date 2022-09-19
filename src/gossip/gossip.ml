open Message

(* TODO: self identified map, such that id -> message[id] always holds *)
type gossip =
  | Gossip of {
      (* TODO: this is clearly not ideal *)
      pending_request : bool;
      consumed : Message_hash.Set.t;
      (* TODO: at max one delayed per connection,
          prevents infinite spam *)
      delayed : string list Message_hash.Map.t;
    }

type fragment =
  | Fragment_encode_message of { content : Message.Content.t }
  | Fragment_decode_message of {
      expected_hash : Message_hash.t;
      raw_content : string;
    }
  | Fragment_send_request of { content : Request.Content.t }
  | Fragment_incoming_request of {
      id : Request_id.t;
      expected_hash : Request_hash.t;
      raw_content : string;
    }
  | Fragment_send_response of {
      id : Request_id.t;
      content : Response.Content.t;
    }
  | Fragment_incoming_response of {
      expected_hash : Response_hash.t;
      raw_content : string;
    }

type outcome =
  | Outcome_message of { message : Message.t; raw_message : Message.raw }
  | Outcome_send_request of { raw_request : Request.raw }
  | Outcome_incoming_request of { id : Request_id.t; request : Request.t }
  | Outcome_send_response of { id : Request_id.t; raw_response : Response.raw }
  | Outcome_incoming_response of { response : Response.t }
  | Outcome_message_decoded_error of { expected_hash : Message_hash.t }
  | Outcome_request_decoded_error of { expected_hash : Request_hash.t }
  | Outcome_response_decoded_error of { expected_hash : Response_hash.t }

type action =
  | Gossip_apply_and_broadcast of {
      message : Message.t;
      raw_message : Message.raw;
    }
  | Gossip_send_request of { raw_request : Request.raw }
  | Gossip_incoming_request of { id : Request_id.t; request : Request.t }
  | Gossip_send_response of { id : Request_id.t; raw_response : Response.raw }
  | Gossip_incoming_response of { response : Response.t }
  | Gossip_fragment of { fragment : fragment }

type t = gossip

let empty =
  Gossip
    {
      pending_request = false;
      consumed = Message_hash.Set.empty;
      delayed = Message_hash.Map.empty;
    }

let broadcast_message ~content = Fragment_encode_message { content }

let incoming_message ~expected_hash ~raw_content gossip =
  let (Gossip { pending_request; consumed; delayed }) = gossip in
  (* Format.printf "incoming.delayed: %d\n%!" (Message_hash.Map.cardinal delayed); *)
  match Message_hash.Map.find_opt expected_hash delayed with
  | Some raw_contents ->
      let delayed =
        let raw_contents = raw_content :: raw_contents in
        Message_hash.Map.add expected_hash raw_contents delayed
      in
      let gossip = Gossip { pending_request; consumed; delayed } in
      (gossip, None)
  | None ->
      let decode = Fragment_decode_message { expected_hash; raw_content } in
      (gossip, Some decode)

let incoming_message ~expected_hash ~raw_content gossip =
  let (Gossip { pending_request = _; consumed; delayed = _ }) = gossip in
  match Message_hash.Set.mem expected_hash consumed with
  | true -> (gossip, None)
  | false -> incoming_message ~expected_hash ~raw_content gossip

let incoming_message ~raw_expected_hash ~raw_content gossip =
  match Message_hash.of_b58 raw_expected_hash with
  | Some expected_hash -> incoming_message ~expected_hash ~raw_content gossip
  | None -> (gossip, None)

let send_request ~content =
  (* TODO: prevent duplicated requests flying *)
  Fragment_send_request { content }

let incoming_request ~id ~raw_expected_hash ~raw_content =
  match Request_hash.of_b58 raw_expected_hash with
  | Some expected_hash ->
      let fragment =
        Fragment_incoming_request { id; expected_hash; raw_content }
      in
      Some fragment
  | None -> None

let send_response ~id ~content = Fragment_send_response { id; content }

let incoming_response ~raw_expected_hash ~raw_content =
  match Response_hash.of_b58 raw_expected_hash with
  | Some expected_hash ->
      let fragment =
        Fragment_incoming_response { expected_hash; raw_content }
      in
      Some fragment
  | None -> None

let compute_encode_message ~content =
  let message, raw_message = Message.encode ~content in
  Outcome_message { message; raw_message }

let compute_decode_message ~expected_hash ~raw_content =
  match Message.decode ~raw_content with
  | Some (message, raw_message) -> (
      let (Message { hash = actual_hash; _ }) = message in
      match Message_hash.equal expected_hash actual_hash with
      | true -> Outcome_message { message; raw_message }
      | false -> Outcome_message_decoded_error { expected_hash })
  | None -> Outcome_message_decoded_error { expected_hash }

let compute_send_request ~content =
  let _request, raw_request = Request.encode ~content in
  Outcome_send_request { raw_request }

let compute_incoming_request ~id ~expected_hash ~raw_content =
  match Request.decode ~raw_content with
  | Some (request, _raw_request) -> (
      let (Request { hash = actual_hash; _ }) = request in
      match Request_hash.equal expected_hash actual_hash with
      | true -> Outcome_incoming_request { id; request }
      | false -> Outcome_request_decoded_error { expected_hash })
  | None -> Outcome_request_decoded_error { expected_hash }

let compute_send_response ~id ~content =
  let _response, raw_response = Response.encode ~content in
  Outcome_send_response { id; raw_response }

let compute_incoming_response ~expected_hash ~raw_content =
  match Response.decode ~raw_content with
  | Some (response, _raw_response) -> (
      let (Response { hash = actual_hash; _ }) = response in
      match Response_hash.equal expected_hash actual_hash with
      | true -> Outcome_incoming_response { response }
      | false -> Outcome_response_decoded_error { expected_hash })
  | None -> Outcome_response_decoded_error { expected_hash }

let compute fragment =
  match fragment with
  | Fragment_encode_message { content } -> compute_encode_message ~content
  | Fragment_decode_message { expected_hash; raw_content } ->
      compute_decode_message ~expected_hash ~raw_content
  | Fragment_send_request { content } -> compute_send_request ~content
  | Fragment_incoming_request { id; expected_hash; raw_content } ->
      compute_incoming_request ~id ~expected_hash ~raw_content
  | Fragment_send_response { id; content } -> compute_send_response ~id ~content
  | Fragment_incoming_response { expected_hash; raw_content } ->
      compute_incoming_response ~expected_hash ~raw_content

let on_message ~message ~raw_message gossip =
  let (Gossip { pending_request; consumed; delayed }) = gossip in
  let (Message { hash; _ }) = message in

  let consumed = Message_hash.Set.add hash consumed in
  let delayed = Message_hash.Map.remove hash delayed in
  let gossip = Gossip { pending_request; consumed; delayed } in

  (* TODO: when encode, apply could be faster *)
  let action = Gossip_apply_and_broadcast { message; raw_message } in
  (gossip, Some action)

let on_message ~message ~raw_message gossip =
  let (Gossip { pending_request = _; consumed; delayed = _ }) = gossip in
  let (Message { hash; _ }) = message in

  match Message_hash.Set.mem hash consumed with
  | true -> (gossip, None)
  | false -> on_message ~message ~raw_message gossip

let on_send_request ~raw_request gossip =
  let (Gossip { pending_request; consumed; delayed }) = gossip in
  match pending_request with
  | true -> (gossip, None)
  | false ->
      let pending_request = true in
      let gossip = Gossip { pending_request; consumed; delayed } in
      let action = Gossip_send_request { raw_request } in
      (gossip, Some action)

let on_incoming_request ~id ~request gossip =
  let action = Gossip_incoming_request { id; request } in
  (gossip, Some action)

let on_send_response ~id ~raw_response gossip =
  let action = Gossip_send_response { id; raw_response } in
  (gossip, Some action)

let on_incoming_response ~response gossip =
  let (Gossip { pending_request = _; consumed; delayed }) = gossip in
  let pending_request = false in
  let gossip = Gossip { pending_request; consumed; delayed } in
  let action = Gossip_incoming_response { response } in
  (gossip, Some action)

let on_message_decoded_error ~expected_hash gossip =
  let (Gossip { pending_request; consumed; delayed }) = gossip in
  match Message_hash.Map.find_opt expected_hash delayed with
  | Some [] ->
      let delayed = Message_hash.Map.remove expected_hash delayed in
      let gossip = Gossip { pending_request; consumed; delayed } in
      (gossip, None)
  | None -> (gossip, None)
  | Some (raw_content :: raw_contents) ->
      let delayed = Message_hash.Map.add expected_hash raw_contents delayed in
      let gossip = Gossip { pending_request; consumed; delayed } in

      let decode = Fragment_decode_message { expected_hash; raw_content } in
      let action = Gossip_fragment { fragment = decode } in
      (gossip, Some action)

let on_request_decoded_error ~expected_hash:_ gossip =
  (* TODO: do something with this error *)
  (gossip, None)

let on_response_decoded_error ~expected_hash:_ gossip =
  (* TODO: do something with this error *)
  (gossip, None)

let apply ~outcome gossip =
  match outcome with
  | Outcome_message { message; raw_message } ->
      on_message ~message ~raw_message gossip
  | Outcome_send_request { raw_request } -> on_send_request ~raw_request gossip
  | Outcome_incoming_request { id; request } ->
      on_incoming_request ~id ~request gossip
  | Outcome_send_response { id; raw_response } ->
      on_send_response ~id ~raw_response gossip
  | Outcome_incoming_response { response } ->
      on_incoming_response ~response gossip
  | Outcome_message_decoded_error { expected_hash } ->
      on_message_decoded_error ~expected_hash gossip
  | Outcome_request_decoded_error { expected_hash } ->
      on_request_decoded_error ~expected_hash gossip
  | Outcome_response_decoded_error { expected_hash } ->
      on_response_decoded_error ~expected_hash gossip
