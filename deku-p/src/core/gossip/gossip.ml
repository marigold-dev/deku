open Deku_concepts
open Message

(* TODO: self identified map, such that id -> message[id] always holds *)
type gossip =
  | Gossip of {
      (* TODO: this is clearly not ideal *)
      pending_request : bool;
      message_pool : Message_pool.t; [@opaque]
    }

and t = gossip [@@deriving yojson]

let encoding =
  let open Data_encoding in
  conv
    (fun (Gossip { pending_request = _; message_pool }) -> message_pool)
    (fun message_pool -> Gossip { pending_request = false; message_pool })
    Message_pool.encoding

type fragment =
  | Fragment_broadcast_message of { fragment : Message_pool.fragment }
  | Fragment_send_message of {
      connection : Connection_id.t;
      fragment : Message_pool.fragment;
    }
  | Fragment_incoming_request of {
      connection : Connection_id.t;
      raw_header : string;
      raw_content : string;
    }

type outcome =
  | Outcome_broadcast_message of { outcome : Message_pool.outcome }
  | Outcome_send_message of {
      connection : Connection_id.t;
      outcome : Message_pool.outcome;
    }
  | Outcome_incoming_request of {
      connection : Connection_id.t;
      above : Level.t;
    }
  | Outcome_incoming_request_header_error of { connection : Connection_id.t }
  | Outcome_incoming_request_content_error of {
      connection : Connection_id.t;
      exn : exn;
    }

type action =
  | Gossip_apply_and_broadcast of {
      content : Message.Content.t;
      network : Message.Network.t;
    }
  | Gossip_send_message of {
      connection : Connection_id.t;
      network : Message.Network.t;
    }
  | Gossip_incoming_request of { connection : Connection_id.t; above : Level.t }
  | Gossip_fragment of { fragment : fragment }

let initial =
  Gossip { pending_request = false; message_pool = Message_pool.initial }

let broadcast_message ~content =
  let fragment = Message_pool.encode ~content in
  Fragment_broadcast_message { fragment }

let incoming_message ~raw_header ~raw_content gossip =
  let (Gossip { pending_request; message_pool }) = gossip in
  let message_pool, fragment =
    Message_pool.decode ~raw_header ~raw_content message_pool
  in
  let gossip = Gossip { pending_request; message_pool } in
  let fragment =
    match fragment with
    | Some fragment -> Some (Fragment_broadcast_message { fragment })
    | None -> None
  in
  (gossip, fragment)

let send_message ~connection ~content =
  let fragment = Message_pool.encode ~content in
  Fragment_send_message { connection; fragment }

let send_request ~above gossip =
  let (Gossip { pending_request; message_pool }) = gossip in
  match pending_request with
  | true -> (gossip, None)
  | false ->
      let (Request { hash = _; above = _; network }) = Request.encode ~above in
      let pending_request = true in
      let gossip = Gossip { pending_request; message_pool } in
      (gossip, Some network)

let incoming_request ~connection ~raw_header ~raw_content =
  Fragment_incoming_request { connection; raw_header; raw_content }

let compute fragment =
  match fragment with
  | Fragment_broadcast_message { fragment } ->
      let outcome = Message_pool.compute fragment in
      Outcome_broadcast_message { outcome }
  | Fragment_send_message { connection; fragment } ->
      let outcome = Message_pool.compute fragment in
      Outcome_send_message { connection; outcome }
  | Fragment_incoming_request { connection; raw_header; raw_content } -> (
      match Request_hash.of_b58 raw_header with
      | Some expected -> (
          try
            let (Request { hash = _; above; network = _ }) =
              Request.decode ~expected ~raw_content
            in
            Outcome_incoming_request { connection; above }
          with exn ->
            Outcome_incoming_request_content_error { connection; exn })
      | None -> Outcome_incoming_request_header_error { connection })

let apply ~outcome gossip =
  let (Gossip { pending_request; message_pool }) = gossip in

  match outcome with
  | Outcome_broadcast_message { outcome } ->
      let message_pool, action = Message_pool.apply ~outcome message_pool in
      let gossip = Gossip { pending_request; message_pool } in
      let action =
        match action with
        | Some (Message_pool_message { message }) ->
            let (Message { header = _; content; network }) = message in
            Some (Gossip_apply_and_broadcast { content; network })
        | Some (Message_pool_fragment { fragment }) ->
            let fragment = Fragment_broadcast_message { fragment } in
            Some (Gossip_fragment { fragment })
        | None -> None
      in
      (gossip, action)
  | Outcome_send_message { connection; outcome } ->
      let message_pool, action = Message_pool.apply ~outcome message_pool in
      let gossip = Gossip { pending_request; message_pool } in
      let action =
        (* TODO: dedup code*)
        match action with
        | Some (Message_pool_message { message }) ->
            let (Message { header = _; content = _; network }) = message in
            Some (Gossip_send_message { connection; network })
        | Some (Message_pool_fragment { fragment }) ->
            let fragment = Fragment_broadcast_message { fragment } in
            Some (Gossip_fragment { fragment })
        | None -> None
      in
      (gossip, action)
  | Outcome_incoming_request { connection; above } ->
      (gossip, Some (Gossip_incoming_request { connection; above }))
  | Outcome_incoming_request_header_error { connection } ->
      Logs.warn (fun m ->
          m "request.header.%a: error" Connection_id.pp connection);
      (gossip, None)
  | Outcome_incoming_request_content_error { connection; exn } ->
      Logs.warn (fun m ->
          m "request.content.%a: %s" Connection_id.pp connection
            (Printexc.to_string exn));
      (gossip, None)

let close ~until gossip =
  let (Gossip { pending_request = _; message_pool }) = gossip in
  (* TODO: if you had progress, you can probably request again *)
  let pending_request = false in
  let message_pool = Message_pool.close ~until message_pool in
  Gossip { pending_request; message_pool }
