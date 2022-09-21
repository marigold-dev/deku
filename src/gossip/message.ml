module Content = struct
  open Deku_concepts
  open Deku_protocol
  open Deku_consensus

  type content =
    | Content_block of Block.t
    | Content_vote of Verified_signature.t
    | Content_operation of Operation.t
  [@@deriving show]

  and t = content [@@deriving yojson]

  let pp = pp_content
  let block block = Content_block block
  let vote vote = Content_vote vote
  let operation operation = Content_operation operation
end

type message = Message of { hash : Message_hash.t; content : Content.t }
type t = message

type raw_message =
  | Raw_message of { hash : Message_hash.t; raw_content : string }

type raw = raw_message

let hash ~json_content =
  (* guarantees canonical representation *)
  let raw_content = Yojson.Safe.to_string json_content in
  let hash = Message_hash.hash raw_content in
  (hash, raw_content)

let encode ~content =
  let json_content = Content.yojson_of_t content in
  let hash, raw_content = hash ~json_content in
  let message = Message { hash; content } in
  let raw_message = Raw_message { hash; raw_content } in
  (message, raw_message)

let decode ~raw_content =
  try
    let json_content = Yojson.Safe.from_string raw_content in
    let hash, raw_content = hash ~json_content in
    let content = Content.t_of_yojson json_content in
    let message = Message { hash; content } in
    let raw_message = Raw_message { hash; raw_content } in
    Some (message, raw_message)
  with _exn -> None (* TODO: dump this exception *)
