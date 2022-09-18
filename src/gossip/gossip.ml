open Deku_crypto
open Message

(* TODO: Timestamp.t *)
type timestamp = float

(* TODO: self identified map, such that id -> message[id] always holds *)
type gossip =
  | Gossip of {
      ready : (Message.raw * timestamp) Message_hash.Map.t;
      (* TODO: at max one delayed per connection,
          prevents infinite spam *)
      delayed : string list Message_hash.Map.t;
      last_cleanup : timestamp;
    }

type fragment =
  | Fragment_encode of { content : Message.Content.t }
  | Fragment_send of { to_ : Key_hash.t; content : Message.Content.t }
  | Fragment_decode of {
      expected_hash : Message_hash.t; [@opaque]
      raw_content : string;
    }
[@@deriving show]

type outcome =
  | Outcome_message of { message : Message.t; raw_message : Message.raw }
  | Outcome_send of { to_ : Key_hash.t; raw_message : Message.raw }
  | Outcome_decoded_error of { expected_hash : Message_hash.t }

type action =
  | Gossip_apply_and_broadcast of {
      message : Message.t;
      raw_message : Message.raw;
    }
  | Gossip_send of { to_ : Key_hash.t; raw_message : Message.raw }
  | Gossip_fragment of { fragment : fragment }

type t = gossip

let empty =
  Gossip
    {
      ready = Message_hash.Map.empty;
      delayed = Message_hash.Map.empty;
      last_cleanup = 0.0;
    }

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
  let (Gossip { ready; delayed; last_cleanup }) = gossip in
  (* Format.printf "incoming.delayed: %d\n%!" (Message_hash.Map.cardinal delayed); *)
  match Message_hash.Map.find_opt expected_hash delayed with
  | Some raw_contents ->
      let delayed =
        let raw_contents = raw_content :: raw_contents in
        Message_hash.Map.add expected_hash raw_contents delayed
      in
      let gossip = Gossip { delayed; ready; last_cleanup } in
      (gossip, None)
  | None ->
      let decode = Fragment_decode { expected_hash; raw_content } in
      (gossip, Some decode)

let incoming ~expected_hash ~raw_content gossip =
  let (Gossip { ready; delayed = _; last_cleanup = _ }) = gossip in
  match Message_hash.Map.mem expected_hash ready with
  | true -> (gossip, None)
  | false -> incoming ~expected_hash ~raw_content gossip

let incoming ~raw_expected_hash ~raw_content gossip =
  match Message_hash.of_b58 raw_expected_hash with
  | Some expected_hash -> incoming ~expected_hash ~raw_content gossip
  | None -> (gossip, None)

let send ~to_ ~content = Fragment_send { to_; content }
let broadcast ~content = Fragment_encode { content }

let on_message ~current ~message ~raw_message gossip =
  let (Gossip { ready; delayed; last_cleanup }) = gossip in
  let (Message { hash; _ }) = message in

  let ready = Message_hash.Map.add hash (raw_message, current) ready in
  let delayed = Message_hash.Map.remove hash delayed in
  let gossip = Gossip { ready; delayed; last_cleanup } in

  (* TODO: when encode, apply could be faster *)
  let action = Gossip_apply_and_broadcast { message; raw_message } in
  (gossip, Some action)

let on_message ~current ~message ~raw_message gossip =
  let (Gossip { ready; delayed = _; last_cleanup = _ }) = gossip in
  let (Message { hash; _ }) = message in

  match Message_hash.Map.mem hash ready with
  | true -> (gossip, None)
  | false -> on_message ~current ~message ~raw_message gossip

let on_send ~to_ ~raw_message gossip =
  let action = Gossip_send { to_; raw_message } in
  (gossip, Some action)

let on_decoded_error ~expected_hash gossip =
  let (Gossip { ready; delayed; last_cleanup }) = gossip in
  match Message_hash.Map.find_opt expected_hash delayed with
  | Some [] ->
      let delayed = Message_hash.Map.remove expected_hash delayed in
      let gossip = Gossip { ready; delayed; last_cleanup } in
      (gossip, None)
  | None -> (gossip, None)
  | Some (raw_content :: raw_contents) ->
      let delayed = Message_hash.Map.add expected_hash raw_contents delayed in
      let gossip = Gossip { ready; delayed; last_cleanup } in

      let decode = Fragment_decode { expected_hash; raw_content } in
      let action = Gossip_fragment { fragment = decode } in
      (gossip, Some action)

let apply ~current ~outcome gossip =
  match outcome with
  | Outcome_message { message; raw_message } ->
      on_message ~current ~message ~raw_message gossip
  | Outcome_send { to_; raw_message } -> on_send ~to_ ~raw_message gossip
  | Outcome_decoded_error { expected_hash } ->
      on_decoded_error ~expected_hash gossip

let compute fragment =
  let compute_encode ~content =
    let message, raw_message = Message.encode ~content in
    Outcome_message { message; raw_message }
  in
  let compute_send ~to_ ~content =
    let _message, raw_message = Message.encode ~content in
    Outcome_send { to_; raw_message }
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
  | Fragment_send { to_; content } -> compute_send ~to_ ~content
  | Fragment_decode { expected_hash; raw_content } ->
      compute_decode ~expected_hash ~raw_content

let clean ~current gossip =
  let (Gossip { ready; delayed; last_cleanup }) = gossip in
  (* Format.printf "gossip.clean\n%!"; *)
  match current -. last_cleanup > Deku_constants.clean_gossip_time with
  | true ->
      (* Format.printf "gossip.cleaning: %d\n%!" (Message_hash.Map.cardinal ready); *)
      let ready =
        Message_hash.Map.filter
          (fun _hash (_message, timestamp) ->
            let time_since_message = current -. timestamp in
            (* Format.printf "message: %b\n%!"
               (time_since_message < Deku_constants.clean_gossip_time); *)
            time_since_message < Deku_constants.clean_gossip_time)
          ready
      in
      (* Format.printf "gossip.cleaned: %d\n%!" (Message_hash.Map.cardinal ready); *)
      let last_cleanup = current in
      Gossip { ready; delayed; last_cleanup }
  | false -> Gossip { ready; delayed; last_cleanup }

let test () =
  let open Deku_consensus in
  let _message, raw_message =
    Message.encode ~content:(Content.block Genesis.block)
  in
  let (Raw_message { hash; _ }) = raw_message in
  let current = 0.0 in
  let gossip =
    let ready = Message_hash.Map.(add hash (raw_message, current) empty) in
    Gossip { ready; delayed = Message_hash.Map.empty; last_cleanup = current }
  in
  let current = current +. Deku_constants.clean_gossip_time in
  let gossip = clean ~current gossip in
  let current = current +. 1.0 in
  let _ = clean ~current gossip in
  ()
