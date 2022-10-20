module Header = struct
  open Deku_concepts

  type header = Message_header of { hash : Message_hash.t; level : Level.t }
  and t = header [@@deriving show, eq, yojson]

  let make ~hash ~level = Message_header { hash; level }

  let encode header =
    let json = yojson_of_header header in
    Yojson.Safe.to_string json

  let decode ~raw_header =
    let json = Yojson.Safe.from_string raw_header in
    header_of_yojson json
end

module Content = struct
  open Deku_concepts
  open Deku_protocol
  open Deku_consensus
  (* TODO: content hash, so that message hashing is fast and we can avoid reencoding *)

  type content =
    | Content_block of Block.t
    | Content_vote of { level : Level.t; vote : Verified_signature.t }
    | Content_operation of Operation.Signed.t
    | Content_accepted of { block : Block.t; votes : Verified_signature.t list }

  and t = content [@@deriving show, yojson]

  exception Invalid_content

  let block block = Content_block block
  let vote ~level ~vote = Content_vote { level; vote }
  let operation operation = Content_operation operation
  let accepted ~block ~votes = Content_accepted { block; votes }

  let last_relevant_level content =
    match content with
    | Content_block block | Content_accepted { block; votes = _ } ->
        let (Block { level; _ }) = block in
        level
    | Content_vote { level; vote = _ } -> level
    | Content_operation operation ->
        let (Signed_operation { initial; _ }) = operation in
        Operation.Initial.last_includable_level initial
end

module Network = struct
  type network_message =
    | Network_message of { raw_header : string; raw_content : string }

  and t = network_message [@@deriving yojson]

  let make ~raw_header ~raw_content =
    Network_message { raw_header; raw_content }
end

type message =
  | Message of { header : Header.t; content : Content.t; network : Network.t }

type t = message

exception Expected_header_mismatch

let hash ~json_content =
  (* guarantees canonical representation *)
  let raw_content = Yojson.Safe.to_string json_content in
  let hash = Message_hash.hash raw_content in
  (hash, raw_content)

let header ~hash ~content =
  let level = Content.last_relevant_level content in
  let header = Header.make ~hash ~level in
  let raw_header = Header.encode header in
  (header, raw_header)

let encode ~content =
  let json_content = Content.yojson_of_t content in
  let hash, raw_content = hash ~json_content in
  let header, raw_header = header ~hash ~content in
  let network = Network.make ~raw_header ~raw_content in
  Message { header; content; network }

let decode ~expected ~raw_content =
  (* TODO: why not use encode here? To avoid reserializing again *)
  let json_content = Yojson.Safe.from_string raw_content in
  let hash, raw_content = hash ~json_content in
  let content = Content.t_of_yojson json_content in
  let header, raw_header = header ~hash ~content in
  let network = Network.make ~raw_header ~raw_content in
  (match Header.equal expected header with
  | true -> ()
  | false -> raise Expected_header_mismatch);
  Message { header; content; network }
