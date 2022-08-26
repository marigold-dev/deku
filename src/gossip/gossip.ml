open Message

(* TODO: self identified map, such that id -> message[id] always holds *)
type gossip =
  | Gossip of {
      ready : Message.raw Message_hash.Map.t;
      (* TODO: at max one delayed per connection,
          prevents infinite spam *)
      delayed : string list Message_hash.Map.t;
    }

type fragment =
  | Fragment_encode of { content : Message.Content.t }
  | Fragment_decode of { expected_hash : Message_hash.t; raw_content : string }

type outcome =
  | Outcome_message of { message : Message.t; raw_message : Message.raw }
  | Outcome_decoded_error of { expected_hash : Message_hash.t }

type action =
  | Gossip_apply_and_broadcast of {
      message : Message.t;
      raw_message : Message.raw;
    }
  | Gossip_fragment of { fragment : fragment }

type t = gossip

let empty =
  Gossip { ready = Message_hash.Map.empty; delayed = Message_hash.Map.empty }

(* TODO:
      let sync ~external_messages gossip =
        (* TODO: n log n *)
        let (Gossip { messages = internal_messages }) = gossip in
        let missing_externally =
          Message_hash.Map.fold (fun message_hash message message_set ->
              match Message_hash.Set.mem message_hash external_messages with
              | true -> message_set
              | false -> Message.Set.add message message_set)
        in
        let missing_internally =
          Message_hash.Set.filter (fun message_hash ->
              not (Message_hash.Map.mem message_hash internal_messages))
        in

        let requests = Message_hash.Set.fold_left in
        () *)

(* external *)
let incoming ~expected_hash ~raw_content gossip =
  let (Gossip { ready; delayed }) = gossip in
  match Message_hash.Map.find_opt expected_hash delayed with
  | Some raw_contents ->
      let delayed =
        let raw_contents = raw_content :: raw_contents in
        Message_hash.Map.add expected_hash raw_contents delayed
      in
      let gossip = Gossip { delayed; ready } in
      (gossip, None)
  | None ->
      let decode = Fragment_decode { expected_hash; raw_content } in
      (gossip, Some decode)

let incoming ~expected_hash ~raw_content gossip =
  let (Gossip { ready; delayed = _ }) = gossip in
  match Message_hash.Map.mem expected_hash ready with
  | true -> (gossip, None)
  | false -> incoming ~expected_hash ~raw_content gossip

let incoming ~raw_expected_hash ~raw_content gossip =
  match Message_hash.of_b58 raw_expected_hash with
  | Some expected_hash -> incoming ~expected_hash ~raw_content gossip
  | None -> (gossip, None)

let broadcast ~content = Fragment_encode { content }

let on_message ~message ~raw_message gossip =
  let (Gossip { ready; delayed }) = gossip in
  let (Message { hash; _ }) = message in

  let ready = Message_hash.Map.add hash raw_message ready in
  let delayed = Message_hash.Map.remove hash delayed in
  let gossip = Gossip { ready; delayed } in

  (* TODO: when encode, apply could be faster *)
  let action = Gossip_apply_and_broadcast { message; raw_message } in
  (gossip, Some action)

let on_message ~message ~raw_message gossip =
  let (Gossip { ready; delayed = _ }) = gossip in
  let (Message { hash; _ }) = message in

  match Message_hash.Map.mem hash ready with
  | true -> (gossip, None)
  | false -> on_message ~message ~raw_message gossip

let on_decoded_error ~expected_hash gossip =
  let (Gossip { ready; delayed }) = gossip in
  match Message_hash.Map.find_opt expected_hash delayed with
  | Some [] | None -> (gossip, None)
  | Some (raw_content :: raw_contents) ->
      let delayed = Message_hash.Map.add expected_hash raw_contents delayed in
      let gossip = Gossip { ready; delayed } in

      let decode = Fragment_decode { expected_hash; raw_content } in
      let action = Gossip_fragment { fragment = decode } in
      (gossip, Some action)

let apply ~outcome gossip =
  match outcome with
  | Outcome_message { message; raw_message } ->
      on_message ~message ~raw_message gossip
  | Outcome_decoded_error { expected_hash } ->
      on_decoded_error ~expected_hash gossip

let compute fragment =
  let compute_encode ~content =
    let message, raw_message = Message.encode ~content in
    Outcome_message { message; raw_message }
  in

  let compute_decode ~expected_hash ~raw_content =
    match Message.decode ~raw_content with
    | Some (message, raw_message) -> (
        let (Message { hash = actual_hash; _ }) = message in
        match Message_hash.equal expected_hash actual_hash with
        | true -> Outcome_message { message; raw_message }
        | false ->
            (* TODO: probably do something here *)
            Outcome_decoded_error { expected_hash })
    | None ->
        (* TODO: probably do something here *)
        Outcome_decoded_error { expected_hash }
  in
  match fragment with
  | Fragment_encode { content } -> compute_encode ~content
  | Fragment_decode { expected_hash; raw_content } ->
      compute_decode ~expected_hash ~raw_content
