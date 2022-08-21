type message = Message of { hash : Message_hash.t; content : Yojson.Safe.t }
type t = message

module Repr = struct
  type json = Yojson.Safe.t

  let json_of_yojson x = x
  let yojson_of_json x = x

  type message = { hash : Message_hash.t; content : json } [@@deriving yojson]

  let hash_content content =
    let content = Yojson.Safe.to_string content in
    Message_hash.hash content
end

let t_of_yojson json =
  let Repr.{ hash; content } = Repr.message_of_yojson json in
  Message { hash; content }

let yojson_of_t message =
  let (Message { hash; content }) = message in
  Repr.yojson_of_message { hash; content }

let make ~content =
  let hash = Repr.hash_content content in
  Message { hash; content }

let verify message =
  let (Message { hash; content }) = message in
  let expected = Repr.hash_content content in
  Message_hash.equal hash expected

open Deku_concepts
open Deku_protocol
open Deku_consensus
open Endpoint

let content_of_yojson (type a) ~(endpoint : a endpoint) json : a =
  match endpoint with
  | Blocks -> Block.t_of_yojson json
  | Signatures -> Verified_signature.t_of_yojson json
  | Operations -> Operation.t_of_yojson json

let yojson_of_content (type a) ~(endpoint : a endpoint) (content : a) =
  match endpoint with
  | Blocks -> Block.yojson_of_t content
  | Signatures -> Verified_signature.yojson_of_t content
  | Operations -> Operation.yojson_of_t content
