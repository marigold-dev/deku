open Deku_concepts

type message_state =
  | Accepted
  | Pending of { queue : string list }
  | Unknown
  | Late
[@@deriving yojson]

type message_pool =
  | Message_pool of {
      current : Level.t;
      by_level : message_state Message_hash.Map.t Level.Map.t;
    }

and t = message_pool [@@deriving yojson]

type fragment =
  | Fragment_encode of { content : Message.Content.t }
  | Fragment_decode of { expected : Message.Header.t; raw_content : string }

type outcome =
  | Outcome_message of { message : Message.t }
  | Outcome_error of { expected : Message.Header.t; exn : exn }

type action =
  | Message_pool_message of { message : Message.t }
  | Message_pool_fragment of { fragment : fragment }

let initial = Message_pool { current = Level.zero; by_level = Level.Map.empty }

(* helpers *)
let rec drop ~level ~until by_level =
  match Level.(until > level) with
  | true ->
      let by_level = Level.Map.remove level by_level in
      let level = Level.next level in
      drop ~level ~until by_level
  | false -> Level.Map.remove until by_level

let drop ~until by_level =
  match Level.Map.min_binding_opt by_level with
  | Some (level, _by_hash) -> drop ~level ~until by_level
  | None -> by_level

let by_hash ~level by_level =
  match Level.Map.find_opt level by_level with
  | Some by_hash -> by_hash
  | None -> Message_hash.Map.empty

let state ~hash ~level pool =
  let (Message_pool { current; by_level }) = pool in
  let by_hash = by_hash ~level by_level in
  match Level.(level > current) with
  | true -> (
      match Message_hash.Map.find_opt hash by_hash with
      | Some state -> state
      | None -> Unknown)
  | false -> Late

let with_state ~hash ~level state pool =
  let (Message_pool { current; by_level }) = pool in
  let by_hash = by_hash ~level by_level in
  let by_hash = Message_hash.Map.add hash state by_hash in
  let by_level = Level.Map.add level by_hash by_level in
  Message_pool { current; by_level }

(* external *)
let encode ~content =
  (* TODO: content hash to prevent double encoding *)
  Fragment_encode { content }

let decode ~raw_header ~raw_content pool =
  let open Message.Header in
  try
    let expected = Message.Header.decode ~raw_header in
    let (Message_header { hash; level }) = expected in
    match state ~hash ~level pool with
    | Accepted -> (pool, None)
    | Pending { queue } ->
        let queue = raw_content :: queue in
        let state = Pending { queue } in
        let pool = with_state ~hash ~level state pool in
        (pool, None)
    | Unknown ->
        let queue = [] in
        let state = Pending { queue } in
        let pool = with_state ~hash ~level state pool in
        let fragment = Fragment_decode { expected; raw_content } in
        (pool, Some fragment)
    | Late -> (pool, None)
  with exn ->
    Logs.warn (fun m -> m "message.header: %s" (Printexc.to_string exn));
    (pool, None)

let compute fragment =
  match fragment with
  | Fragment_encode { content } ->
      let message = Message.encode ~content in
      Outcome_message { message }
  | Fragment_decode { expected; raw_content } -> (
      try
        let message = Message.decode ~expected ~raw_content in
        Outcome_message { message }
      with exn -> Outcome_error { expected; exn })

let apply ~outcome pool =
  (* TODO: be resilient to double apply *)
  match outcome with
  | Outcome_message { message } -> (
      let (Message { header; _ }) = message in
      let (Message_header { hash; level }) = header in
      match state ~hash ~level pool with
      | Pending _ | Unknown ->
          let pool = with_state ~hash ~level Accepted pool in
          let message = Message_pool_message { message } in
          (pool, Some message)
      | Accepted | Late -> (pool, None))
  | Outcome_error { expected; exn } -> (
      Logs.warn (fun m -> m "outcome.error: %s" (Printexc.to_string exn));
      let (Message_header { hash; level }) = expected in
      match state ~hash ~level pool with
      | Pending { queue } -> (
          match queue with
          | [] ->
              let pool = with_state ~hash ~level Unknown pool in
              (pool, None)
          | raw_content :: queue ->
              (* TODO: dedup logic *)
              let state = Pending { queue } in
              let pool = with_state ~hash ~level state pool in
              let fragment = Fragment_decode { expected; raw_content } in
              let fragment = Message_pool_fragment { fragment } in
              (pool, Some fragment))
      | Accepted | Unknown | Late -> (pool, None))

let close ~until pool =
  let (Message_pool { current = _; by_level }) = pool in
  (* TODO: this allows to reopen (close ~ until:Level.initial) *)
  (* TODO: worst case scenario this becomes n log n,
     probably limit the drop until *)
  let by_level = drop ~until by_level in
  Message_pool { current = until; by_level }
