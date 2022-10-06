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
    | Content_operation of Operation.t
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
    | Content_operation operation -> Operation.last_includable_level operation

  (* TODO: this is a hack*)
  let encode content =
    match content with
    | Content_block block ->
        let tag = "block" in
        tag :: Block.encode block
    | Content_vote _ ->
        let tag = "vote" in
        let json = yojson_of_content content in
        [ tag; Yojson.Safe.to_string json ]
    | Content_operation _ ->
        let tag = "operation" in
        let json = yojson_of_content content in
        [ tag; Yojson.Safe.to_string json ]
    | Content_accepted { block; votes } ->
        let tag = "accepted" in
        let votes = [%yojson_of: Verified_signature.t list] votes in
        tag :: Yojson.Safe.to_string votes :: Block.encode block

  let decode fragments =
    match fragments with
    | "block" :: fragments ->
        let block = Block.decode fragments in
        Content_block block
    | [ ("vote" | "operation"); content ] ->
        let json = Yojson.Safe.from_string content in
        content_of_yojson json
    | "accepted" :: votes :: fragments ->
        let votes =
          let json = Yojson.Safe.from_string votes in
          [%of_yojson: Verified_signature.t list] json
        in
        let block = Block.decode fragments in
        Content_accepted { block; votes }
    | _ -> raise Invalid_content
end

module Network = struct
  type network_message =
    | Network_message of { raw_header : string; raw_fragments : string list }

  and t = network_message [@@deriving yojson]

  let make ~raw_header ~raw_fragments =
    Network_message { raw_header; raw_fragments }
end

type message =
  | Message of { header : Header.t; content : Content.t; network : Network.t }

type t = message

exception Expected_header_mismatch

let hash ~raw_fragments =
  (* guarantees canonical representation *)
  let raw_content = String.concat ":" raw_fragments in
  Message_hash.hash raw_content

let header ~hash ~content =
  let level = Content.last_relevant_level content in
  let header = Header.make ~hash ~level in
  let raw_header = Header.encode header in
  (header, raw_header)

let encode ~content =
  let raw_fragments = Content.encode content in
  let hash = hash ~raw_fragments in
  let header, raw_header = header ~hash ~content in
  let network = Network.make ~raw_header ~raw_fragments in
  Message { header; content; network }

let decode ~expected ~raw_fragments =
  (* TODO: why not use encode here? To avoid reserializing again *)
  let hash = hash ~raw_fragments in
  let content = Content.decode raw_fragments in
  let header, raw_header = header ~hash ~content in
  let network = Network.make ~raw_header ~raw_fragments in
  (match Header.equal expected header with
  | true -> ()
  | false -> raise Expected_header_mismatch);
  Message { header; content; network }
