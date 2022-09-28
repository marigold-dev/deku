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

and t = gossip [@@deriving yojson]

type fragment =
  | Fragment_encode_message of { content : Message.Content.t }
  | Fragment_decode_message of {
      expected_hash : Message_hash.t;
      raw_content : string;
    }
  | Fragment_send_message of {
      connection : Connection_id.t;
      content : Message.Content.t;
    }
  | Fragment_send_request of { content : Request.Content.t }
  | Fragment_incoming_request of {
      connection : Connection_id.t;
      expected_hash : Request_hash.t;
      raw_content : string;
    }

type outcome =
  | Outcome_message of { message : Message.t; raw_message : Message.raw }
  | Outcome_send_message of {
      connection : Connection_id.t;
      raw_message : Message.raw;
    }
  | Outcome_send_request of { raw_request : Request.raw }
  | Outcome_incoming_request of {
      connection : Connection_id.t;
      request : Request.t;
    }
  | Outcome_message_decoded_error of { expected_hash : Message_hash.t }
  | Outcome_request_decoded_error of { expected_hash : Request_hash.t }

type action =
  | Gossip_apply_and_broadcast of {
      message : Message.t;
      raw_message : Message.raw;
    }
  | Gossip_send_message of {
      connection : Connection_id.t;
      raw_message : Message.raw;
    }
  | Gossip_send_request of { raw_request : Request.raw }
  | Gossip_incoming_request of {
      connection : Connection_id.t;
      request : Request.t;
    }
  | Gossip_fragment of { fragment : fragment }

let empty =
  Gossip
    {
      pending_request = false;
      consumed = Message_hash.Set.empty;
      delayed = Message_hash.Map.empty;
    }

let broadcast_message ~content =
  (* let open Deku_concepts in
     let open Deku_consensus in
     (match content with
     | Message.Content.Content_block block ->
         let (Block.Block { hash; level; _ }) = block in
         Format.eprintf "broadcast.block(%a)(%.3f): %s\n%!" Level.pp level
           (Unix.gettimeofday ()) (Block_hash.to_b58 hash)
     | Content_vote vote ->
         let key_hash = Verified_signature.key_hash vote in
         Format.eprintf "broadcast.vote(%.3f): %s\n%!" (Unix.gettimeofday ())
           (Deku_crypto.Key_hash.to_b58 key_hash)
     | Content_operation _ -> ()); *)
  Fragment_encode_message { content }

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

let send_message ~connection ~content =
  Fragment_send_message { connection; content }

let send_request ~content =
  (* TODO: prevent duplicated requests flying *)
  Fragment_send_request { content }

let incoming_request ~connection ~raw_expected_hash ~raw_content =
  match Request_hash.of_b58 raw_expected_hash with
  | Some expected_hash ->
      let fragment =
        Fragment_incoming_request { connection; expected_hash; raw_content }
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

let compute_send_message ~connection ~content =
  let _message, raw_message = Message.encode ~content in
  Outcome_send_message { connection; raw_message }

let compute_send_request ~content =
  let _request, raw_request = Request.encode ~content in
  Outcome_send_request { raw_request }

let compute_incoming_request ~connection ~expected_hash ~raw_content =
  match Request.decode ~raw_content with
  | Some (request, _raw_request) -> (
      let (Request { hash = actual_hash; _ }) = request in
      match Request_hash.equal expected_hash actual_hash with
      | true -> Outcome_incoming_request { connection; request }
      | false -> Outcome_request_decoded_error { expected_hash })
  | None -> Outcome_request_decoded_error { expected_hash }

let compute fragment =
  match fragment with
  | Fragment_encode_message { content } -> compute_encode_message ~content
  | Fragment_decode_message { expected_hash; raw_content } ->
      compute_decode_message ~expected_hash ~raw_content
  | Fragment_send_message { connection; content } ->
      compute_send_message ~connection ~content
  | Fragment_send_request { content } -> compute_send_request ~content
  | Fragment_incoming_request { connection; expected_hash; raw_content } ->
      compute_incoming_request ~connection ~expected_hash ~raw_content

let on_message ~message ~raw_message gossip =
  let (Gossip { pending_request; consumed; delayed }) = gossip in
  let (Message { hash; _ }) = message in

  let pending_request =
    (* TODO: this is a hack but should work for this case *)
    let (Message { hash = _; content }) = message in
    match content with
    | Content_block _ | Content_vote _ | Content_operation _ -> pending_request
    | Content_accepted _ -> false
  in
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

let on_send_message ~connection ~raw_message gossip =
  let action = Gossip_send_message { connection; raw_message } in
  (gossip, Some action)

let on_send_request ~raw_request gossip =
  let (Gossip { pending_request; consumed; delayed }) = gossip in
  match pending_request with
  | true -> (gossip, None)
  | false ->
      let pending_request = true in
      let gossip = Gossip { pending_request; consumed; delayed } in
      let action = Gossip_send_request { raw_request } in
      (gossip, Some action)

let on_incoming_request ~connection ~request gossip =
  let action = Gossip_incoming_request { connection; request } in
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

let apply ~outcome gossip =
  match outcome with
  | Outcome_message { message; raw_message } ->
      on_message ~message ~raw_message gossip
  | Outcome_send_message { connection; raw_message } ->
      on_send_message ~connection ~raw_message gossip
  | Outcome_send_request { raw_request } -> on_send_request ~raw_request gossip
  | Outcome_incoming_request { connection; request } ->
      on_incoming_request ~connection ~request gossip
  | Outcome_message_decoded_error { expected_hash } ->
      on_message_decoded_error ~expected_hash gossip
  | Outcome_request_decoded_error { expected_hash } ->
      on_request_decoded_error ~expected_hash gossip

let clear gossip =
  let (Gossip gossip) = gossip in
  let pending_request = false in
  let delayed = Message_hash.Map.empty in
  Gossip { gossip with pending_request; delayed }
