type packet = Packet of { hash : Packet_hash.t; content : Yojson.Safe.t }
type t = packet

module Repr = struct
  type json = Yojson.Safe.t

  let json_of_yojson x = x
  let yojson_of_json x = x

  type packet = { hash : Packet_hash.t; content : json } [@@deriving yojson]

  let hash_content content =
    let content = Yojson.Safe.to_string content in
    Packet_hash.hash content
end

let t_of_yojson json =
  let Repr.{ hash; content } = Repr.packet_of_yojson json in
  Packet { hash; content }

let yojson_of_t packet =
  let (Packet { hash; content }) = packet in
  Repr.yojson_of_packet { hash; content }

let make ~content =
  let hash = Repr.hash_content content in
  Packet { hash; content }

let verify packet =
  let (Packet { hash; content }) = packet in
  let expected = Repr.hash_content content in
  Packet_hash.equal hash expected

open Deku_concepts
open Deku_protocol
open Deku_consensus
open Endpoint

let content_of_yojson (type a) ~(endpoint : a endpoint) json : a =
  match endpoint with
  | Blocks -> Block.t_of_yojson json
  | Signatures -> Verified_signature.t_of_yojson json
  | Operations -> Operation.t_of_yojson json
  | Bootstrap -> Bootstrap_signal.t_of_yojson json
  | Withdraw_proof -> Operation_hash.t_of_yojson json
  | Level -> Level.t_of_yojson json

let yojson_of_content (type a) ~(endpoint : a endpoint) (content : a) =
  match endpoint with
  | Blocks -> Block.yojson_of_t content
  | Signatures -> Verified_signature.yojson_of_t content
  | Operations -> Operation.yojson_of_t content
  | Bootstrap -> Bootstrap_signal.yojson_of_t content
  | Withdraw_proof -> Operation_hash.yojson_of_t content
  | Level -> Level.yojson_of_t content
