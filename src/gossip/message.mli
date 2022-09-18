module Content : sig
  open Deku_crypto
  open Deku_concepts
  open Deku_protocol
  open Deku_consensus

  type content = private
    | Content_block of Block.t
    | Content_vote of Verified_signature.t
    | Content_operation of Operation.t
    | Content_request_block of { to_ : Key_hash.t; hash : Block_hash.t }

  type t = content [@@deriving yojson]

  val block : Block.t -> content
  val vote : Verified_signature.t -> content
  val operation : Operation.t -> content
  val request_block : to_:Key_hash.t -> hash:Block_hash.t -> content
end

type message = private
  | Message of { hash : Message_hash.t; content : Content.t }

type t = message

type raw_message = private
  | Raw_message of { hash : Message_hash.t; raw_content : string }

type raw = raw_message

val encode : content:Content.t -> message * raw_message
val decode : raw_content:string -> (message * raw_message) option
